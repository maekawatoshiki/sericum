// TODO: much legacy code remains.

use super::super::register::{PhysReg, RegisterClassKind};
use super::roundup;
use crate::codegen::x64::machine::{
    basic_block::*, const_data::*, frame_object::*, function::*, inst::*, module::*,
};
use crate::ir;
use crate::{codegen::internal_function_names, ir::types::*};
use dynasmrt::*;
use rustc_hash::FxHashMap;

#[rustfmt::skip]
macro_rules! register {
    ($reg:expr) => {{
        phys_reg_to_dynasm_reg($reg.get_reg().unwrap())
    }};
}

fn phys_reg_to_dynasm_reg(r: PhysReg) -> u8 {
    (r.retrieve() - r.reg_class() as usize) as u8
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericValue {
    Int32(i32),
    F64(f64),
    None,
}

pub struct JITExecutor {
    jit: JITCompiler,
    machine_module: MachineModule,
}

pub struct JITCompiler {
    asm: x64::Assembler,
    labels: FxHashMap<LabelKey, DynamicLabel>,
    internal_functions: FxHashMap<String, u64>, // name -> fn address
}

impl JITExecutor {
    pub fn new(module: &ir::module::Module) -> Self {
        use super::super::{dag, machine};

        let mut dag_module = dag::convert::ConvertToDAG::new(module).convert_module();

        debug!(println!("dag: before: {:?}", dag_module));

        dag::combine::Combine::new().combine_module(&mut dag_module);
        debug!(println!("dag: comibine: {:?}", dag_module));
        dag::legalize::Legalize::new().run_on_module(&mut dag_module);
        debug!(println!("dag: legalize: {:?}", dag_module));
        dag::isel::MISelector::new().run_on_module(&mut dag_module);
        debug!(println!("dag: isel: {:?}", dag_module));

        let mut machine_module = dag::mc_convert::MIConverter::new().convert_module(dag_module);

        // debug!(println!("{:?}", machine_module));

        machine::phi_elimination::PhiElimination::new().run_on_module(&mut machine_module); //
        machine::two_addr::TwoAddressConverter::new().run_on_module(&mut machine_module);
        machine::regalloc::RegisterAllocator::new().run_on_module(&mut machine_module); //
        machine::pro_epi_inserter::PrologueEpilogueInserter::new()
            .run_on_module(&mut machine_module);
        machine::replace_data::ConstDataReplacer::new().run_on_module(&mut machine_module);
        machine::replace_copy::ReplaceCopyWithProperMInst::new().run_on_module(&mut machine_module);

        debug!(println!("{:?}", machine_module));

        // use crate::codegen::x64::asm::print::MachineAsmPrinter;
        // let mut printer = MachineAsmPrinter::new();
        // printer.run_on_module(&machine_module);
        // println!("ASM DUMP: \n{}\n\n", printer.output);

        let mut jit = JITCompiler::new();
        jit.compile_module(&machine_module);

        Self {
            machine_module,
            jit,
        }
    }

    pub fn find_function_by_name(&self, name: &str) -> Option<MachineFunctionId> {
        self.machine_module.find_function_by_name(name)
    }

    pub fn run(&mut self, id: MachineFunctionId, args: Vec<GenericValue>) -> GenericValue {
        self.jit.run(&self.machine_module, id, args)
    }
}

impl JITCompiler {
    pub fn new() -> Self {
        Self {
            asm: x64::Assembler::new().unwrap(),
            labels: FxHashMap::default(),
            internal_functions: {
                let internal_names = internal_function_names();
                let internals = vec![
                    cilk_memset_p0i32_i32_ as u64,
                    cilk_println_i32_ as _,
                    cilk_printch_i32_ as _,
                ];
                assert!(
                    internal_names.len() == internals.len(),
                    "unimplemented internal function"
                );
                internal_names
                    .iter()
                    .map(|n| n.to_string())
                    .zip(internals)
                    .into_iter()
                    .collect::<FxHashMap<_, _>>()
            },
        }
    }

    pub fn run(
        &mut self,
        module: &MachineModule,
        id: MachineFunctionId,
        args: Vec<GenericValue>,
    ) -> GenericValue {
        let f_entry = self.get_label(id);
        let entry = self.asm.offset();

        for (idx, arg) in args.iter().enumerate() {
            match arg {
                GenericValue::Int32(i) => dynasm!(self.asm; mov Ra(phys_reg_to_dynasm_reg(
                    RegisterClassKind::GR32.get_nth_arg_reg(idx).unwrap())), *i),
                GenericValue::F64(_) => unimplemented!(),
                GenericValue::None => unreachable!(),
            }
        }

        dynasm!(self.asm
                ; call =>f_entry
                ; ret);

        self.asm.commit();
        let executor = self.asm.reader();
        let buf = executor.lock();
        let f: extern "C" fn() -> u64 = unsafe { ::std::mem::transmute(buf.ptr(entry)) };

        match module
            .types
            .as_function_ty(module.function_ref(id).ty)
            .unwrap()
            .ret_ty
        {
            Type::Int32 => GenericValue::Int32(f() as i32),
            Type::F64 => {
                let f: extern "C" fn() -> f64 = unsafe { ::std::mem::transmute(buf.ptr(entry)) };
                GenericValue::F64(f() as f64)
            }
            Type::Void => {
                f();
                GenericValue::None
            }
            _ => unimplemented!(),
        }
    }

    pub fn compile_module(&mut self, module: &MachineModule) {
        // Place constant data in memory
        for (id, c) in module.const_data.id_and_data() {
            let x = unsafe { ::std::mem::transmute::<f64, u64>(c.as_f64()) }; // TODO: now support only for f64
            let h = (x >> 32) as i32;
            let l = (x & 0xffff_ffff) as i32;
            let label = self.get_label(id);
            dynasm!(self.asm; =>label; .dword l, h);
        }

        for (f_id, _) in &module.functions {
            self.compile_function(module, f_id);
        }
    }

    fn compile_function(&mut self, module: &MachineModule, id: MachineFunctionId) {
        let f = module.function_ref(id);

        if f.internal {
            return;
        }

        let f_entry = self.get_label(id);

        let frame_objects = FrameObjectsInfo::new(&module.types, f);

        dynasm!(self.asm
            ; =>f_entry
        );

        for (bb_id, bb) in f.body.basic_blocks.id_and_block() {
            if bb_id.index() != 0 {
                let label = self.get_label(bb_id);
                dynasm!(self.asm; =>label);
            }

            for inst in &*bb.iseq_ref() {
                let inst = &f.body.inst_arena[*inst];
                match inst.opcode {
                    MachineOpcode::MOVri32 => self.compile_mov_ri32(inst),
                    MachineOpcode::MOVrr32 => self.compile_mov_rr32(inst),
                    MachineOpcode::MOVri64 => self.compile_mov_ri64(inst),
                    MachineOpcode::MOVrr64 => self.compile_mov_rr64(inst),
                    MachineOpcode::MOVrm32 => self.compile_mov_rm32(&frame_objects, inst),
                    MachineOpcode::MOVrm64 => self.compile_mov_rm64(&frame_objects, inst),
                    MachineOpcode::MOVmr32 => self.compile_mov_mr32(&frame_objects, inst),
                    MachineOpcode::MOVmi32 => self.compile_mov_mi32(&frame_objects, inst),
                    MachineOpcode::MOVSXDr64m32 => self.compile_movsxd_r64m32(&frame_objects, inst),
                    MachineOpcode::MOVSDrm64 => self.compile_movsd_rm64(inst),
                    MachineOpcode::LEAr64m => self.compile_lea_r64m(&frame_objects, inst),
                    MachineOpcode::RET => self.compile_ret(),
                    MachineOpcode::PUSH64 => self.compile_push64(inst),
                    MachineOpcode::POP64 => self.compile_pop64(inst),
                    MachineOpcode::ADDrr32 => self.compile_add_rr32(inst),
                    MachineOpcode::ADDri32 => self.compile_add_ri32(inst),
                    MachineOpcode::ADDr64i32 => self.compile_add_r64i32(inst),
                    MachineOpcode::SUBrr32 => self.compile_sub_rr32(inst),
                    MachineOpcode::SUBri32 => self.compile_sub_ri32(inst),
                    MachineOpcode::SUBr64i32 => self.compile_sub_r64i32(inst),
                    MachineOpcode::IMULrr32 => self.compile_imul_rr32(inst),
                    MachineOpcode::IMULrri32 => self.compile_imul_rri32(inst),
                    MachineOpcode::IMULrr64i32 => self.compile_imul_rr64i32(inst),
                    MachineOpcode::IDIV => self.compile_idiv(&frame_objects, inst),
                    MachineOpcode::CDQ => self.compile_cdq(&frame_objects, inst),
                    MachineOpcode::CALL => self.compile_call(module, &frame_objects, inst),
                    MachineOpcode::Copy => self.compile_copy(inst),
                    MachineOpcode::CMPri => self.compile_cmp_ri(inst),
                    MachineOpcode::CMPrr => self.compile_cmp_rr(inst),
                    MachineOpcode::JE => self.compile_je(inst),
                    MachineOpcode::JLE => self.compile_jle(inst),
                    MachineOpcode::JL => self.compile_jl(inst),
                    MachineOpcode::JMP => self.compile_jmp(inst),
                    MachineOpcode::Ret => self.compile_return(&frame_objects, inst),
                    op => unimplemented!("{:?}", op),
                }
            }
        }
    }

    fn compile_mov_rm32(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        // mov rbp, fi, none, none
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = inst.operand[1].as_frame_index();
            dynasm!(self.asm; mov Rd(r0), [rbp - fo.offset(m1.idx).unwrap()]);
        }

        // out = mov rbp, fi, none, const.off
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_const_i32()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i2 = inst.operand[3].as_constant().as_i32();
            dynasm!(self.asm; mov Rd(r0), [rbp - m1 + i2]);
        }

        // out = mov rbp, fi, align, off
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i2 = inst.operand[2].as_constant().as_i32();
            let r3 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());

            match i2 {
                4 => dynasm!(self.asm; mov Rd(r0), DWORD [rbp - m1 + 4*Rq(r3)]),
                _ => unimplemented!(),
            }
        }

        // out = mov base, none, align, off
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i2 = inst.operand[2].as_constant().as_i32();
            let r3 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());

            match i2 {
                4 => dynasm!(self.asm; mov Rd(r0), DWORD [Rq(r1) + 4*Rq(r3)]),
                _ => unimplemented!(),
            }
        }

        // out = mov base, none, none, const.off
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_none()
            && inst.operand[3].is_const_i32()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i2 = inst.operand[3].as_constant().as_i32();
            dynasm!(self.asm; mov Rd(r0), DWORD [Rq(r1) + i2]);
        }

        // out = mov base, none, none, none
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            dynasm!(self.asm; mov Rd(r0), [Rq(r1)]);
        }
    }

    fn compile_mov_rm64(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        // mov rbp, fi, none, none
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = inst.operand[1].as_frame_index();
            dynasm!(self.asm; mov Rq(r0), QWORD [rbp - fo.offset(m1.idx).unwrap()]);
        }

        // out = mov rbp, fi, none, const.off
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_const_i32()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i2 = inst.operand[3].as_constant().as_i32();
            dynasm!(self.asm; mov Rq(r0), QWORD [rbp - m1 + i2]);
        }

        // out = mov rbp, fi, align, off
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i2 = inst.operand[2].as_constant().as_i32();
            let r3 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());

            match i2 {
                4 => dynasm!(self.asm; mov Rq(r0), QWORD [rbp - m1 + 4*Rq(r3)]),
                _ => unimplemented!(),
            }
        }

        // out = mov base, none, align, off
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i2 = inst.operand[2].as_constant().as_i32();
            let r3 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());

            match i2 {
                4 => dynasm!(self.asm; mov Rq(r0), QWORD [Rq(r1) + 4*Rq(r3)]),
                _ => unimplemented!(),
            }
        }

        // out = mov base, none, none, none
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            dynasm!(self.asm; mov Rq(r0), QWORD [Rq(r1)]);
        }
    }

    fn compile_mov_mr32(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        // mov rbp, fi, none, none, r
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
            && inst.operand[4].is_register()
        {
            let m0 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let r1 = phys_reg_to_dynasm_reg(inst.operand[4].as_register().get_reg().unwrap());
            dynasm!(self.asm; mov DWORD [rbp - m0], Rd(r1));
        }

        // mov rbp, fi, none, const.off, r
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_const_i32()
            && inst.operand[4].is_register()
        {
            let m0 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i1 = inst.operand[3].as_constant().as_i32();
            let r2 = phys_reg_to_dynasm_reg(inst.operand[4].as_register().get_reg().unwrap());
            dynasm!(self.asm; mov DWORD [rbp - m0 + i1], Rd(r2));
        }

        // mov rbp, fi, align, off, r
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
            && inst.operand[4].is_register()
        {
            let m0 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i1 = inst.operand[2].as_constant().as_i32();
            let r2 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());
            let r3 = phys_reg_to_dynasm_reg(inst.operand[4].as_register().get_reg().unwrap());
            match i1 {
                4 => dynasm!(self.asm; mov DWORD [rbp - m0 + 4*Rq(r2)], Rd(r3)),
                _ => unimplemented!(),
            }
        }

        // mov base, none, align, off, r
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
            && inst.operand[4].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i1 = inst.operand[2].as_constant().as_i32();
            let r2 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());
            let r3 = phys_reg_to_dynasm_reg(inst.operand[4].as_register().get_reg().unwrap());
            match i1 {
                4 => dynasm!(self.asm; mov DWORD [Rq(r0) + 4*Rq(r2)], Rd(r3)),
                _ => unimplemented!(),
            }
        }

        // mov base, none, none, const.off, r
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_none()
            && inst.operand[3].is_const_i32()
            && inst.operand[4].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i1 = inst.operand[3].as_constant().as_i32();
            let r2 = phys_reg_to_dynasm_reg(inst.operand[4].as_register().get_reg().unwrap());
            dynasm!(self.asm; mov DWORD [Rq(r0) + i1], Rd(r2))
        }

        // mov base, none, none, none, r
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
            && inst.operand[4].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let r1 = phys_reg_to_dynasm_reg(inst.operand[4].as_register().get_reg().unwrap());
            dynasm!(self.asm; mov DWORD [Rq(r0)], Rd(r1));
        }
    }

    fn compile_mov_mi32(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        // mov rbp, fi, none, none, r
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
            && inst.operand[4].is_const_i32()
        {
            let m0 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i1 = inst.operand[4].as_constant().as_i32();
            dynasm!(self.asm; mov DWORD [rbp - m0], i1);
        }

        // out = mov rbp, fi, none, const.off, r
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_const_i32()
            && inst.operand[4].is_const_i32()
        {
            let m0 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i1 = inst.operand[3].as_constant().as_i32();
            let i2 = inst.operand[4].as_constant().as_i32();
            dynasm!(self.asm; mov DWORD [rbp - m0 + i1], i2);
        }

        // out = mov rbp, fi, align, off, r
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
            && inst.operand[4].is_const_i32()
        {
            let m0 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i1 = inst.operand[2].as_constant().as_i32();
            let r2 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());
            let i3 = inst.operand[4].as_constant().as_i32();
            match i1 {
                4 => dynasm!(self.asm; mov DWORD [rbp - m0 + 4*Rq(r2)], i3),
                _ => unimplemented!(),
            }
        }

        // out = mov base, none, align, off, r
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
            && inst.operand[4].is_const_i32()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i1 = inst.operand[2].as_constant().as_i32();
            let r2 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());
            let i3 = inst.operand[4].as_constant().as_i32();
            match i1 {
                4 => dynasm!(self.asm; mov DWORD [Rq(r0) + 4*Rq(r2)], i3),
                _ => unimplemented!(),
            }
        }

        // mov base, none, none, const.off, r
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_none()
            && inst.operand[3].is_const_i32()
            && inst.operand[4].is_const_i32()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i1 = inst.operand[3].as_constant().as_i32();
            let i2 = inst.operand[4].as_constant().as_i32();
            dynasm!(self.asm; mov DWORD [Rq(r0) + i1], i2);
        }

        // out = mov base, none, none, none, r
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
            && inst.operand[4].is_const_i32()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i1 = inst.operand[4].as_constant().as_i32();
            dynasm!(self.asm; mov DWORD [Rq(r0)], i1);
        }
    }

    fn compile_mov_ri32(&mut self, inst: &MachineInst) {
        assert!(matches!(inst.operand[0], MachineOperand::Constant(_)));
        assert!(matches!(
            inst.operand[0].as_constant(),
            MachineConstant::Int32(_)
        ));
        let r = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let i = inst.operand[0].as_constant().as_i32();
        dynasm!(self.asm; mov Rd(r), i);
    }

    fn compile_mov_rr32(&mut self, inst: &MachineInst) {
        assert!(matches!(inst.operand[0], MachineOperand::Register(_)));
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
        self.reg_copy(RegisterClassKind::GR32, r0, r1);
    }

    fn compile_mov_ri64(&mut self, _inst: &MachineInst) {
        unimplemented!()
        // assert!(matches!(inst.operand[0], MachineOperand::Constant(_)));
        // assert!(matches!(
        //     inst.operand[0].as_constant(),
        //     MachineConstant::Int64(_)
        // ));
        //
        // let r = inst.def[0].get_reg().unwrap().get() as u8;
        // let i = inst.operand[0].as_constant().as_i64();
        // dynasm!(self.asm; mov Ra(r), i);
    }

    fn compile_mov_rr64(&mut self, inst: &MachineInst) {
        assert!(matches!(inst.operand[0], MachineOperand::Register(_)));
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
        self.reg_copy(RegisterClassKind::GR64, r0, r1);
    }

    fn compile_movsd_rm64(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let m1 = inst.operand[0].as_address().as_absolute();
        let l1 = self.get_label(m1);
        dynasm!(self.asm; movsd Rx(r0), [=>l1]);
    }

    fn compile_lea_r64m(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        // out = lea rbp, fi, none, none
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = inst.operand[1].as_frame_index();
            dynasm!(self.asm; lea Rq(r0), [rbp - fo.offset(m1.idx).unwrap()]);
        }

        // out = lea rbp, fi, none, const.off
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_none()
            && inst.operand[3].is_const_i32()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i2 = inst.operand[3].as_constant().as_i32();
            dynasm!(self.asm; lea Rq(r0), [rbp - m1 + i2]);
        }

        // out = lea rbp, fi, align, off
        if inst.operand[0].is_register() // must be rbp
            && inst.operand[1].is_frame_index()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let m1 = fo.offset(inst.operand[1].as_frame_index().idx).unwrap();
            let i2 = inst.operand[2].as_constant().as_i32();
            let r3 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());

            match i2 {
                1 => dynasm!(self.asm; lea Rq(r0), [rbp - m1 + Rq(r3)]),
                _ => unimplemented!(),
            }
        }

        // out = lea base, none, align, off
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_const_i32()
            && inst.operand[3].is_register()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            let i2 = inst.operand[2].as_constant().as_i32();
            let r3 = phys_reg_to_dynasm_reg(inst.operand[3].as_register().get_reg().unwrap());

            match i2 {
                1 => dynasm!(self.asm; lea Rq(r0), [Rq(r1) + Rq(r3)]),
                _ => unimplemented!(),
            }
        }

        // out = lea base, none, none, none
        if inst.operand[0].is_register()
            && inst.operand[1].is_none()
            && inst.operand[2].is_none()
            && inst.operand[3].is_none()
        {
            let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
            let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
            dynasm!(self.asm; lea Rq(r0), [Rq(r1)]);
        }
    }

    fn compile_push64(&mut self, inst: &MachineInst) {
        let op0 = &inst.operand[0];
        match op0 {
            MachineOperand::Register(reg) => dynasm!(self.asm; push Rq(register!(reg))),
            _ => unimplemented!(),
        }
    }

    fn compile_pop64(&mut self, inst: &MachineInst) {
        let op0 = &inst.operand[0];
        match op0 {
            MachineOperand::Register(reg) => dynasm!(self.asm; pop Rq(register!(reg))),
            _ => unimplemented!(),
        }
    }

    fn compile_copy(&mut self, inst: &MachineInst) {
        let rn = register!(inst);
        let op0 = &inst.operand[0];
        match op0 {
            MachineOperand::Register(reg) => self.reg_copy(reg.get_reg_class(), rn, register!(reg)),
            _ => unimplemented!(),
        }
    }

    fn compile_cmp_ri(&mut self, inst: &MachineInst) {
        let bits = inst.operand[0]
            .as_register()
            .get_reg()
            .unwrap()
            .reg_class()
            .size_in_bits();
        let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
        let i1 = inst.operand[1].as_constant().as_i32();
        match bits {
            32 => dynasm!(self.asm; cmp Rd(r0), i1),
            64 => dynasm!(self.asm; cmp Rq(r0), i1),
            _ => unimplemented!(),
        }
    }

    fn compile_cmp_rr(&mut self, inst: &MachineInst) {
        let bits = inst.operand[0]
            .as_register()
            .get_reg()
            .unwrap()
            .reg_class()
            .size_in_bits();
        let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().get_reg().unwrap());
        match bits {
            32 => dynasm!(self.asm; cmp Rd(r0), Rd(r1)),
            64 => dynasm!(self.asm; cmp Rq(r0), Rq(r1)),
            _ => unimplemented!(),
        }
    }

    fn compile_je(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; je => l);
    }

    fn compile_jle(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; jle => l);
    }

    fn compile_jl(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; jl => l);
    }

    fn compile_movsxd_r64m32(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let m1 = fo.offset(inst.operand[0].as_frame_index().idx).unwrap();
        dynasm!(self.asm; movsxd Rq(r0), [rbp - m1]);
    }

    fn compile_call(&mut self, module: &MachineModule, _fo: &FrameObjectsInfo, inst: &MachineInst) {
        let callee_id = module
            .find_function_by_name(match &inst.operand[0] {
                MachineOperand::Address(AddressInfo::FunctionName(n)) => n.as_str(),
                _ => unimplemented!(),
            })
            .unwrap();
        let callee_entity = module.function_ref(callee_id);
        // let ret_ty = &callee_entity.ty.get_function_ty().unwrap().ret_ty;
        let rsp_offset = 0;

        // rsp_offset += self.push_args(f, args);
        self.assign_args_to_regs(&inst.operand[1..]);

        if callee_entity.internal {
            let callee = self.internal_functions.get(&callee_entity.name).unwrap();
            let rsp_adjust = roundup(rsp_offset as i32, 16) - rsp_offset as i32; // rsp is 16-byte aligned
            dynasm!(self.asm
                ; sub rsp, rsp_adjust
                ; mov rax, QWORD *callee as _
                ; call rax
                ; add rsp, rsp_adjust
            );
        } else {
            let f_entry = self.get_label(callee_id);
            dynasm!(self.asm; call => f_entry);
        }

        // match ret_ty {
        //     Type::Int32 => self.reg_copy(ret_ty, register!(inst), 0),
        //     Type::Void => {}
        //     _ => unimplemented!(),
        // }

        // for save_reg in save_regs.iter().rev() {
        //     dynasm!(self.asm; pop Ra(*save_reg));
        // }
    }

    fn compile_add_rr32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().get_reg().unwrap());
        dynasm!(self.asm; add Rd(r0), Rd(r1));
    }

    fn compile_add_ri32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let i1 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; add Rd(r0), i1);
    }

    fn compile_add_r64i32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let i1 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; add Rq(r0), i1);
    }

    fn compile_sub_rr32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().get_reg().unwrap());
        dynasm!(self.asm; sub Rd(r0), Rd(r1));
    }

    fn compile_sub_ri32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let i1 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; sub Rd(r0), i1);
    }

    fn compile_sub_r64i32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let i1 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; sub Rq(r0), i1);
    }

    fn compile_imul_rr32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().get_reg().unwrap());
        dynasm!(self.asm; imul Rd(r0), Rd(r1))
    }

    fn compile_imul_rri32(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
        let i2 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; imul Rd(r0), Rd(r1), i2);
    }

    fn compile_imul_rr64i32(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().get_reg().unwrap());
        let i2 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; imul Rq(r0), Rq(r1), i2);
    }

    fn compile_cdq(&mut self, _fo: &FrameObjectsInfo, _inst: &MachineInst) {
        dynasm!(self.asm; cdq)
    }

    fn compile_idiv(&mut self, _fo: &FrameObjectsInfo, inst: &MachineInst) {
        let (r, r_ty) = {
            let reg = inst.operand[0].as_register();
            (
                phys_reg_to_dynasm_reg(reg.info_ref().reg.unwrap()),
                &reg.info_ref().reg_class,
            )
        };
        match r_ty {
            RegisterClassKind::GR32 => dynasm!(self.asm; idiv Rd(r)),
            _ => unimplemented!(),
        }
    }

    fn compile_jmp(&mut self, inst: &MachineInst) {
        match &inst.operand[0] {
            MachineOperand::Branch(bb) => {
                let label = self.get_label(*bb);
                dynasm!(self.asm; jmp =>label)
            }
            _ => unimplemented!(),
        }
    }

    fn compile_ret(&mut self) {
        dynasm!(self.asm; ret);
    }

    fn compile_return(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        match &inst.operand[0] {
            MachineOperand::Constant(c) => match c {
                MachineConstant::Int32(i) => dynasm!(self.asm; mov rax, *i),
                _ => unimplemented!(),
            },
            MachineOperand::Register(i) => dynasm!(self.asm; mov rax, Ra(register!(i))),
            MachineOperand::FrameIndex(fi) => {
                dynasm!(self.asm; mov rax, [rbp - fo.offset(fi.idx).unwrap()])
            }
            MachineOperand::None => {}
            _ => unreachable!(),
        }
        dynasm!(self.asm; mov rsp, rbp; pop rbp; ret);
    }

    fn assign_args_to_regs(&mut self, args: &[MachineOperand]) {
        for (idx, arg) in args.iter().enumerate() {
            match arg {
                MachineOperand::Constant(MachineConstant::Int32(i)) => {
                    dynasm!(self.asm; mov Rd(phys_reg_to_dynasm_reg(
                                RegisterClassKind::GR32.get_nth_arg_reg(idx).unwrap())), *i)
                }
                MachineOperand::Register(id) => self.reg_copy(
                    id.get_reg_class(),
                    phys_reg_to_dynasm_reg(id.get_reg_class().get_nth_arg_reg(idx).unwrap()),
                    register!(id),
                ),
                _ => unimplemented!(),
            }
        }
    }

    // TODO: r0 and r1 should be PhysReg not u8
    fn reg_copy(&mut self, rc: RegisterClassKind, r0: u8, r1: u8) {
        if r0 == r1 {
            return;
        }

        match rc {
            RegisterClassKind::GR32 => dynasm!(self.asm; mov Rd(r0), Rd(r1)),
            RegisterClassKind::GR64 => dynasm!(self.asm; mov Rq(r0), Rq(r1)),
            _ => unimplemented!(),
        }
    }

    fn get_label<K: Into<LabelKey>>(&mut self, key: K) -> DynamicLabel {
        let key = key.into();

        if let Some(label) = self.labels.get(&key) {
            return *label;
        }

        let new_label = self.asm.new_dynamic_label();
        self.labels.insert(key, new_label);
        new_label
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
enum LabelKey {
    Data(DataId),
    BB(MachineBasicBlockId),
    Func(MachineFunctionId),
}

impl From<MachineBasicBlockId> for LabelKey {
    fn from(id: MachineBasicBlockId) -> Self {
        LabelKey::BB(id)
    }
}

impl From<DataId> for LabelKey {
    fn from(id: DataId) -> Self {
        LabelKey::Data(id)
    }
}

impl From<MachineFunctionId> for LabelKey {
    fn from(id: MachineFunctionId) -> Self {
        LabelKey::Func(id)
    }
}

// Internal function cilk.println.i32
#[no_mangle]
pub extern "C" fn cilk_println_i32_(i: i32) {
    println!("{}", i);
}

// EXPERIMENTAL Internal function cilk.printch.i32
#[no_mangle]
pub extern "C" fn cilk_printch_i32_(ch: i32) {
    print!("{}", ch as u8 as char);
}

// EXPERIMENTAL Internal function cilk.memset.p0i32.i32
#[no_mangle]
pub extern "C" fn cilk_memset_p0i32_i32_(p: *mut i32, x: i32, count: i32) {
    unsafe { ::std::ptr::write_bytes(p, x as u8, count as usize) }
}

#[test]
fn test_phys_reg_to_dynasm_reg() {
    use super::super::register::*;
    assert_eq!(phys_reg_to_dynasm_reg(GR32::EAX.as_phys_reg()), 0);
    assert_eq!(phys_reg_to_dynasm_reg(GR64::RAX.as_phys_reg()), 0);
    assert_eq!(phys_reg_to_dynasm_reg(GR32::R15D.as_phys_reg()), 15);
    assert_eq!(phys_reg_to_dynasm_reg(GR64::R15.as_phys_reg()), 15);
}
