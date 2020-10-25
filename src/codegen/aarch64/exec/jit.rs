// TODO: much legacy code remains.

use super::super::machine::register::{PhysReg, RegisterClassKind};
use crate::{
    codegen::{
        internal_function_names,
        x64::{
            machine::{
                basic_block::*, const_data::*, frame_object::*, function::*, inst::*, module::*,
            },
            standard_conversion_into_machine_module,
        },
    },
    ir,
    ir::types::*,
};
use dynasmrt::*;
use rustc_hash::FxHashMap;

#[rustfmt::skip]
macro_rules! register {
    ($reg:expr) => {{
        phys_reg_to_dynasm_reg($reg.as_phys_reg())
    }};
}

fn phys_reg_to_dynasm_reg(r: PhysReg) -> u8 {
    (r.retrieve() - r.reg_class() as usize) as u8
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericValue {
    Address(*mut u8),
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
    pub fn new(module: &mut ir::module::Module) -> Self {
        let machine_module = standard_conversion_into_machine_module(module);
        // println!("{:?}", machine_module);

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
        let now = ::std::time::Instant::now();
        let res = self.jit.run(&self.machine_module, id, args);
        debug!(println!(
            "duration: {:?}",
            ::std::time::Instant::now().duration_since(now)
        ));
        res
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
                    sericum_memset_p0i32_i32_ as u64,
                    sericum_println_i32_ as _,
                    sericum_print_i32_ as _,
                    sericum_printch_i32_ as _,
                    sericum_println_f64_ as _,
                    sericum_print_f64_ as _,
                    sericum_sin_f64_ as _,
                    sericum_cos_f64_ as _,
                    sericum_sqrt_f64_ as _,
                    sericum_floor_f64_ as _,
                    sericum_fabs_f64_ as _,
                    sericum_i32_to_f64_i32_ as _,
                    sericum_f64_to_i32_f64_ as _,
                    sericum_malloc_i32_ as _,
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
                GenericValue::Int32(i) => dynasm!(self.asm; mov Rd(phys_reg_to_dynasm_reg(
                    RegisterClassKind::GR32.get_nth_arg_reg(idx).unwrap())), *i),
                GenericValue::F64(_) => unimplemented!(),
                GenericValue::Address(addr) => dynasm!(self.asm; mov Rq(phys_reg_to_dynasm_reg(
                                RegisterClassKind::GR64.get_nth_arg_reg(idx).unwrap())), QWORD *addr as i64),
                GenericValue::None => unreachable!(),
            }
        }

        dynasm!(self.asm
                ; sub rsp, 8
                ; call =>f_entry
                ; add rsp, 8
                ; ret);

        self.asm.commit();
        let executor = self.asm.reader();
        let buf = executor.lock();
        let f: extern "C" fn() -> u64 = unsafe { ::std::mem::transmute(buf.ptr(entry)) };

        match module
            .types
            .base
            .borrow()
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

        if f.is_internal {
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
                    MachineOpcode::MOVmr64 => self.compile_mov_mr64(&frame_objects, inst),
                    MachineOpcode::MOVmi32 => self.compile_mov_mi32(&frame_objects, inst),
                    MachineOpcode::MOVSXDr64m32 => self.compile_movsxd_r64m32(&frame_objects, inst),
                    MachineOpcode::MOVSDrm64 => self.compile_movsd_rm64(inst),
                    MachineOpcode::MOVSDrm => self.compile_movsd_rm(&frame_objects, inst),
                    MachineOpcode::MOVSDmr => self.compile_movsd_mr(&frame_objects, inst),
                    MachineOpcode::MOVSDrr => self.compile_movsd_rr(inst),
                    MachineOpcode::LEAr64m => self.compile_lea_r64m(&frame_objects, inst),
                    MachineOpcode::RET => self.compile_ret(),
                    MachineOpcode::PUSH64 => self.compile_push64(inst),
                    MachineOpcode::POP64 => self.compile_pop64(inst),
                    MachineOpcode::ADDrr32 => self.compile_add_rr32(inst),
                    MachineOpcode::ADDrr64 => self.compile_add_rr64(inst),
                    MachineOpcode::ADDri32 => self.compile_add_ri32(inst),
                    MachineOpcode::ADDr64i32 => self.compile_add_r64i32(inst),
                    MachineOpcode::ADDSDrr => self.compile_addsd_rr(inst),
                    MachineOpcode::ADDSDrm => self.compile_addsd_rm(&frame_objects, inst),
                    MachineOpcode::SUBrr32 => self.compile_sub_rr32(inst),
                    MachineOpcode::SUBri32 => self.compile_sub_ri32(inst),
                    MachineOpcode::SUBr64i32 => self.compile_sub_r64i32(inst),
                    MachineOpcode::SUBSDrr => self.compile_subsd_rr(inst),
                    MachineOpcode::SUBSDrm => self.compile_subsd_rm(&frame_objects, inst),
                    MachineOpcode::IMULrr32 => self.compile_imul_rr32(inst),
                    MachineOpcode::IMULrri32 => self.compile_imul_rri32(inst),
                    MachineOpcode::IMULrr64i32 => self.compile_imul_rr64i32(inst),
                    MachineOpcode::MULSDrr => self.compile_mulsd_rr(inst),
                    MachineOpcode::MULSDrm => self.compile_mulsd_rm(&frame_objects, inst),
                    MachineOpcode::DIVSDrr => self.compile_divsd_rr(inst),
                    MachineOpcode::DIVSDrm => self.compile_divsd_rm(&frame_objects, inst),
                    MachineOpcode::SQRTSDrr => self.compile_sqrtsd_rr(inst),
                    MachineOpcode::IDIV => self.compile_idiv(&frame_objects, inst),
                    MachineOpcode::CDQ => self.compile_cdq(&frame_objects, inst),
                    MachineOpcode::SHLr32i8 => self.compile_shl_r32i8(inst),
                    MachineOpcode::SHLr64i8 => self.compile_shl_r64i8(inst),
                    MachineOpcode::CALL => self.compile_call(module, &frame_objects, inst),
                    MachineOpcode::CMPri => self.compile_cmp_ri(inst),
                    MachineOpcode::CMPrr => self.compile_cmp_rr(inst),
                    MachineOpcode::UCOMISDrr => self.compile_ucomisd_rr(inst),
                    MachineOpcode::JE => self.compile_je(inst),
                    MachineOpcode::JBE => self.compile_jbe(inst),
                    MachineOpcode::JB => self.compile_jb(inst),
                    MachineOpcode::JLE => self.compile_jle(inst),
                    MachineOpcode::JL => self.compile_jl(inst),
                    MachineOpcode::JAE => self.compile_jae(inst),
                    MachineOpcode::JA => self.compile_ja(inst),
                    MachineOpcode::JG => self.compile_jg(inst),
                    MachineOpcode::JGE => self.compile_jge(inst),
                    MachineOpcode::JMP => self.compile_jmp(inst),
                    MachineOpcode::Ret => self.compile_return(&frame_objects, inst),
                    op => unimplemented!("{:?}", op),
                }
            }
        }
    }

    fn compile_mov_rm32(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fi.idx;
                dynasm!(self.asm; mov Rd(r0), DWORD [Rq(r1) - fo.offset(m2).unwrap()]);
            }
            MachineOperand::Mem(MachineMemOperand::BaseAlignOff(base, align, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i2 = align;
                let r3 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                match i2 {
                    4 => dynasm!(self.asm; mov Rd(r0), DWORD [Rq(r1) + 4*Rq(r3)]),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fo.offset(fi.idx).unwrap();
                let i3 = align;
                let r4 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                match i3 {
                    4 => dynasm!(self.asm; mov Rd(r0), DWORD [Rq(r1) - m2 + 4*Rq(r4)]),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fo.offset(fi.idx).unwrap();
                let i3 = off;
                dynasm!(self.asm; mov Rd(r0), DWORD [Rq(r1) - m2 + i3]);
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                dynasm!(self.asm; mov Rd(r0), DWORD [Rq(r1)]);
            }
            MachineOperand::Mem(MachineMemOperand::BaseOff(base, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i2 = *off;
                dynasm!(self.asm; mov Rd(r0), DWORD [Rq(r1) + i2]);
            }
            e => panic!("{:?}", e),
        }
    }

    fn compile_mov_rm64(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fi.idx;
                dynasm!(self.asm; mov Rq(r0), QWORD [Rq(r1) - fo.offset(m2).unwrap()]);
            }
            MachineOperand::Mem(MachineMemOperand::BaseAlignOff(base, align, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i2 = align;
                let r3 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                match i2 {
                    4 => dynasm!(self.asm; mov Rq(r0), QWORD [Rq(r1) + 4*Rq(r3)]),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fo.offset(fi.idx).unwrap();
                let i3 = align;
                let r4 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                match i3 {
                    4 => dynasm!(self.asm; mov Rq(r0), QWORD [Rq(r1) - m2 + 4*Rq(r4)]),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fo.offset(fi.idx).unwrap();
                let i3 = off;
                dynasm!(self.asm; mov Rq(r0), QWORD [Rq(r1) - m2 + i3]);
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                dynasm!(self.asm; mov Rq(r0), QWORD [Rq(r1)]);
            }
            MachineOperand::Mem(MachineMemOperand::BaseOff(base, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i2 = *off;
                dynasm!(self.asm; mov Rq(r0), QWORD [Rq(r1) + i2]);
            }
            _ => panic!(),
        }
    }

    fn compile_mov_mr32(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let r2 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; mov DWORD [Rq(r0) - m1], Rd(r2));
            }
            MachineOperand::Mem(MachineMemOperand::BaseAlignOff(base, align, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i1 = align;
                let r2 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                let r3 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                match i1 {
                    4 => dynasm!(self.asm; mov DWORD [Rq(r0) + 4*Rq(r2)], Rd(r3)),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let i2 = align;
                let r3 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                let r4 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                match i2 {
                    4 => dynasm!(self.asm; mov DWORD [Rq(r0) - m1 + 4*Rq(r3)], Rd(r4)),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let i2 = off;
                let r3 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; mov QWORD [Rq(r0) - m1 + i2], Rq(r3));
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; mov DWORD [Rq(r0)], Rd(r1));
            }
            MachineOperand::Mem(MachineMemOperand::BaseOff(base, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i1 = *off;
                let r2 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; mov DWORD [Rq(r0) + i1], Rd(r2))
            }
            e => panic!("{:?}", e),
        }
    }

    fn compile_mov_mi32(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let i2 = inst.operand[1].as_constant().as_i32();
                dynasm!(self.asm; mov DWORD [Rq(r0) - m1], i2);
            }
            MachineOperand::Mem(MachineMemOperand::BaseAlignOff(base, align, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i1 = align;
                let r2 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                let i3 = inst.operand[1].as_constant().as_i32();
                match i1 {
                    4 => dynasm!(self.asm; mov DWORD [Rq(r0) + 4*Rq(r2)], i3),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let i2 = align;
                let r3 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                let i4 = inst.operand[1].as_constant().as_i32();
                match i2 {
                    4 => dynasm!(self.asm; mov DWORD [Rq(r0) - m1 + 4*Rq(r3)], i4),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let i2 = off;
                let i3 = inst.operand[1].as_constant().as_i32();
                dynasm!(self.asm; mov QWORD [Rq(r0) - m1 + i2], i3);
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i1 = inst.operand[1].as_constant().as_i32();
                dynasm!(self.asm; mov DWORD [Rq(r0)], i1);
            }
            MachineOperand::Mem(MachineMemOperand::BaseOff(base, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i1 = *off;
                let i2 = inst.operand[1].as_constant().as_i32();
                dynasm!(self.asm; mov DWORD [Rq(r0) + i1], i2)
            }
            e => panic!("{:?}", e),
        }
    }

    fn compile_mov_ri32(&mut self, inst: &MachineInst) {
        assert!(matches!(inst.operand[0], MachineOperand::Constant(_)));
        assert!(matches!(
            inst.operand[0].as_constant(),
            MachineConstant::Int32(_)
        ));
        let r = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let i = inst.operand[0].as_constant().as_i32();
        dynasm!(self.asm; mov Rd(r), i);
    }

    fn compile_mov_rr32(&mut self, inst: &MachineInst) {
        assert!(matches!(inst.operand[0], MachineOperand::Register(_)));
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
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
        // let r = inst.def[0].as_phys_reg().get() as u8;
        // let i = inst.operand[0].as_constant().as_i64();
        // dynasm!(self.asm; mov Ra(r), i);
    }

    fn compile_mov_mr64(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let r2 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; mov QWORD [Rq(r0) - m1], Rq(r2));
            }
            MachineOperand::Mem(MachineMemOperand::BaseAlignOff(base, align, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i1 = align;
                let r2 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                let r3 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                match i1 {
                    4 => dynasm!(self.asm; mov QWORD [Rq(r0) + 4*Rq(r2)], Rq(r3)),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let i2 = align;
                let r3 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                let r4 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                match i2 {
                    4 => dynasm!(self.asm; mov QWORD [Rq(r0) - m1 + 4*Rq(r3)], Rq(r4)),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let i2 = off;
                let r3 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; mov QWORD [Rq(r0) - m1 + i2], Rq(r3));
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; mov QWORD [Rq(r0)], Rq(r1));
            }
            MachineOperand::Mem(MachineMemOperand::BaseOff(base, off)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i1 = *off;
                let r2 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; mov QWORD [Rq(r0) + i1], Rq(r2))
            }
            e => panic!("{:?}", e),
        }
    }

    fn compile_mov_rr64(&mut self, inst: &MachineInst) {
        assert!(matches!(inst.operand[0], MachineOperand::Register(_)));
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
        self.reg_copy(RegisterClassKind::GR64, r0, r1);
    }

    fn compile_movsd_rm64(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::Address(AddressKind::Label(id))) => {
                let l1 = self.get_label(*id);
                dynasm!(self.asm; movsd Rx(r0), [=>l1]);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_movsd_rr(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
        dynasm!(self.asm; movsd Rx(r0), Rx(r1));
    }

    fn compile_movsd_rm(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fi.idx;
                dynasm!(self.asm; movsd Rx(r0), [Rq(r1) - fo.offset(m2).unwrap()]);
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                dynasm!(self.asm; movsd Rx(r0), [Rq(r1)]);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_movsd_mr(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m1 = fo.offset(fi.idx).unwrap();
                let r2 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; movsd [Rq(r0) - m1], Rx(r2));
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let r0 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
                dynasm!(self.asm; movsd [Rq(r0)], Rx(r1));
            }
            _ => unimplemented!(),
        }
    }

    fn compile_lea_r64m(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[0] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fi.idx;
                dynasm!(self.asm; lea Rq(r0), [Rq(r1) - fo.offset(m2).unwrap()]);
            }
            MachineOperand::Mem(MachineMemOperand::BaseAlignOff(base, align, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i2 = align;
                let r3 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                match i2 {
                    1 => dynasm!(self.asm; lea Rq(r0), [Rq(r1) + 1*Rq(r3)]),
                    4 => dynasm!(self.asm; lea Rq(r0), [Rq(r1) + 4*Rq(r3)]),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fo.offset(fi.idx).unwrap();
                let i3 = align;
                let r4 = phys_reg_to_dynasm_reg(off.as_phys_reg());
                match i3 {
                    1 => dynasm!(self.asm; lea Rq(r0), [Rq(r1) - m2 + 1*Rq(r4)]),
                    4 => dynasm!(self.asm; lea Rq(r0), [Rq(r1) - m2 + 4*Rq(r4)]),
                    _ => unimplemented!(),
                }
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fo.offset(fi.idx).unwrap();
                let i3 = off;
                dynasm!(self.asm; lea Rq(r0), [Rq(r1) - m2 + i3]);
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                dynasm!(self.asm; lea Rq(r0), [Rq(r1)]);
            }
            MachineOperand::Mem(MachineMemOperand::BaseOff(base, off)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let i2 = *off;
                dynasm!(self.asm; lea Rq(r0),[Rq(r1) + i2]);
            }
            _ => panic!(),
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

    // fn compile_copy(&mut self, inst: &MachineInst) {
    //     let rn = phys_reg_to_dynasm_reg(inst.get_reg().unwrap());
    //     let op0 = &inst.operand[0];
    //     match op0 {
    //         MachineOperand::Register(reg) => {
    //             self.reg_copy(reg.get_reg_class(), rn, register!(reg))
    //         }
    //         _ => unimplemented!(),
    //     }
    // }

    fn compile_cmp_ri(&mut self, inst: &MachineInst) {
        let bits = inst.operand[0]
            .as_register()
            .as_phys_reg()
            .reg_class()
            .size_in_bits();
        let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
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
            .as_phys_reg()
            .reg_class()
            .size_in_bits();
        let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        match bits {
            32 => dynasm!(self.asm; cmp Rd(r0), Rd(r1)),
            64 => dynasm!(self.asm; cmp Rq(r0), Rq(r1)),
            _ => unimplemented!(),
        }
    }

    fn compile_ucomisd_rr(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; ucomisd Rx(r0), Rx(r1));
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

    fn compile_jbe(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; jbe => l);
    }

    fn compile_jb(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; jb => l);
    }

    fn compile_jg(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; jg => l);
    }

    fn compile_jge(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; jge => l);
    }

    fn compile_ja(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; ja => l);
    }

    fn compile_jae(&mut self, inst: &MachineInst) {
        let l = self.get_label(inst.operand[0].as_basic_block());
        dynasm!(self.asm; jae => l);
    }

    fn compile_movsxd_r64m32(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let m1 = fo.offset(inst.operand[0].as_frame_index().idx).unwrap();
        dynasm!(self.asm; movsxd Rq(r0), [rbp - m1]);
    }

    fn compile_call(&mut self, module: &MachineModule, _fo: &FrameObjectsInfo, inst: &MachineInst) {
        let callee_id = module
            .find_function_by_name(match &inst.operand[0] {
                MachineOperand::Mem(MachineMemOperand::Address(AddressKind::FunctionName(n))) => {
                    n.as_str()
                }
                _ => unimplemented!(),
            })
            .unwrap();
        let callee_entity = module.function_ref(callee_id);

        if callee_entity.is_internal {
            let callee = self.internal_functions.get(&callee_entity.name).unwrap();
            dynasm!(self.asm
                ; mov rax, QWORD *callee as _
                ; call rax
            );
        } else {
            let f_entry = self.get_label(callee_id);
            dynasm!(self.asm; call => f_entry);
        }
    }

    fn compile_add_rr32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; add Rd(r0), Rd(r1));
    }

    fn compile_add_rr64(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; add Rq(r0), Rq(r1));
    }

    fn compile_add_ri32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let i1 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; add Rd(r0), i1);
    }

    fn compile_add_r64i32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let i1 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; add Rq(r0), i1);
    }

    fn compile_addsd_rr(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; addsd Rx(r0), Rx(r1));
    }

    fn compile_addsd_rm(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[1] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fi.idx;
                dynasm!(self.asm; addsd Rx(r0), [Rq(r1) - fo.offset(m2).unwrap()]);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_sub_rr32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; sub Rd(r0), Rd(r1));
    }

    fn compile_sub_ri32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let i1 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; sub Rd(r0), i1);
    }

    fn compile_sub_r64i32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let i1 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; sub Rq(r0), i1);
    }

    fn compile_subsd_rr(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; subsd Rx(r0), Rx(r1));
    }

    fn compile_subsd_rm(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[1] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fi.idx;
                dynasm!(self.asm; subsd Rx(r0), [Rq(r1) - fo.offset(m2).unwrap()]);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_imul_rr32(&mut self, inst: &MachineInst) {
        // inst.operand[0] must be the same as inst.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; imul Rd(r0), Rd(r1))
    }

    fn compile_imul_rri32(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
        let i2 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; imul Rd(r0), Rd(r1), i2);
    }

    fn compile_imul_rr64i32(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
        let i2 = inst.operand[1].as_constant().as_i32();
        dynasm!(self.asm; imul Rq(r0), Rq(r1), i2);
    }

    fn compile_mulsd_rr(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; mulsd Rx(r0), Rx(r1));
    }

    fn compile_mulsd_rm(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[1] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fi.idx;
                dynasm!(self.asm; mulsd Rx(r0), [Rq(r1) - fo.offset(m2).unwrap()]);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_divsd_rr(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[1].as_register().as_phys_reg());
        dynasm!(self.asm; divsd Rx(r0), Rx(r1));
    }

    fn compile_divsd_rm(&mut self, fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        match &inst.operand[1] {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let r1 = phys_reg_to_dynasm_reg(base.as_phys_reg());
                let m2 = fi.idx;
                dynasm!(self.asm; divsd Rx(r0), [Rq(r1) - fo.offset(m2).unwrap()]);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_sqrtsd_rr(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let r1 = phys_reg_to_dynasm_reg(inst.operand[0].as_register().as_phys_reg());
        dynasm!(self.asm; sqrtsd Rx(r0), Rx(r1));
    }

    fn compile_cdq(&mut self, _fo: &FrameObjectsInfo, _inst: &MachineInst) {
        dynasm!(self.asm; cdq)
    }

    fn compile_idiv(&mut self, _fo: &FrameObjectsInfo, inst: &MachineInst) {
        let r = {
            let reg = inst.operand[0].as_register();
            phys_reg_to_dynasm_reg(reg.as_phys_reg())
        };
        dynasm!(self.asm; idiv Rd(r)) // TODO: for Rq
    }

    fn compile_shl_r32i8(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let i1 = inst.operand[1].as_constant().as_i8();
        dynasm!(self.asm; shl Rd(r0), i1);
    }

    fn compile_shl_r64i8(&mut self, inst: &MachineInst) {
        let r0 = phys_reg_to_dynasm_reg(inst.def[0].as_phys_reg());
        let i1 = inst.operand[1].as_constant().as_i8();
        dynasm!(self.asm; shl Rq(r0), i1);
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

// Internal function sericum.println.i32
#[no_mangle]
pub extern "C" fn sericum_println_i32_(i: i32) {
    println!("{}", i);
}

// Internal function sericum.println.f64
#[no_mangle]
pub extern "C" fn sericum_print_i32_(i: i32) {
    print!("{}", i);
}

// Internal function sericum.println.f64
#[no_mangle]
pub extern "C" fn sericum_println_f64_(f: f64) {
    println!("{}", f);
}

// Internal function sericum.println.f64
#[no_mangle]
pub extern "C" fn sericum_print_f64_(f: f64) {
    print!("{}", f);
}

// EXPERIMENTAL Internal function sericum.printch.i32
#[no_mangle]
pub extern "C" fn sericum_printch_i32_(ch: i32) {
    print!("{}", ch as u8 as char);
}

// EXPERIMENTAL Internal function sericum.sin.f64
#[no_mangle]
pub extern "C" fn sericum_sin_f64_(x: f64) -> f64 {
    x.sin()
}

// EXPERIMENTAL Internal function sericum.cos.f64
#[no_mangle]
pub extern "C" fn sericum_cos_f64_(x: f64) -> f64 {
    x.cos()
}

// EXPERIMENTAL Internal function sericum.sqrt.f64
#[no_mangle]
pub extern "C" fn sericum_sqrt_f64_(x: f64) -> f64 {
    x.sqrt()
}

// EXPERIMENTAL Internal function sericum.floor.f64
#[no_mangle]
pub extern "C" fn sericum_floor_f64_(x: f64) -> f64 {
    x.floor()
}

// EXPERIMENTAL Internal function sericum.floor.f64
#[no_mangle]
pub extern "C" fn sericum_fabs_f64_(x: f64) -> f64 {
    x.abs()
}

// EXPERIMENTAL Internal function sericum.floor.f64
#[no_mangle]
pub extern "C" fn sericum_f64_to_i32_f64_(x: f64) -> i32 {
    x as i32
}

// EXPERIMENTAL Internal function sericum.floor.f64
#[no_mangle]
pub extern "C" fn sericum_i32_to_f64_i32_(x: i32) -> f64 {
    x as f64
}

// EXPERIMENTAL Internal function sericum.sqrt.f64
#[no_mangle]
pub extern "C" fn sericum_malloc_i32_(i: i32) -> *mut i64 {
    let mut vec = Vec::<u8>::with_capacity(i as usize);
    unsafe {
        vec.set_len(i as usize);
    }
    Box::into_raw(vec.into_boxed_slice()) as *mut i64
}

// EXPERIMENTAL Internal function sericum.memset.p0i32.i32
#[no_mangle]
pub extern "C" fn sericum_memset_p0i32_i32_(p: *mut i32, x: i32, count: i32) {
    unsafe { ::std::ptr::write_bytes(p, x as u8, count as usize) }
}

#[test]
fn test_phys_reg_to_dynasm_reg() {
    use super::super::machine::register::*;
    assert_eq!(phys_reg_to_dynasm_reg(GR32::EAX.as_phys_reg()), 0);
    assert_eq!(phys_reg_to_dynasm_reg(GR64::RAX.as_phys_reg()), 0);
    assert_eq!(phys_reg_to_dynasm_reg(GR32::R15D.as_phys_reg()), 15);
    assert_eq!(phys_reg_to_dynasm_reg(GR64::R15.as_phys_reg()), 15);
}
