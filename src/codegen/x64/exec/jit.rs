// TODO: much legacy code remains.

use super::super::register::{PhysReg, RegisterClassKind};
use super::roundup;
use crate::codegen::x64::machine::{
    basic_block::*, frame_object::*, function::*, instr::*, module::*,
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
    None,
}

pub struct JITExecutor {
    jit: JITCompiler,
    machine_module: MachineModule,
}

pub struct JITCompiler {
    asm: x64::Assembler,
    function_map: FxHashMap<MachineFunctionId, DynamicLabel>,
    bb_to_label: FxHashMap<MachineBasicBlockId, DynamicLabel>,
    internal_functions: FxHashMap<String, u64>, // name -> fn address
}

impl JITExecutor {
    pub fn new(module: &ir::module::Module) -> Self {
        use super::super::{dag, machine};

        let mut dag_module = dag::convert::ConvertToDAG::new(module).convert_module();

        dag::combine::Combine::new().combine_module(&mut dag_module);
        dag::isel::MISelector::new().run_on_module(&mut dag_module);

        debug!(
            println!("DAG:");
            for (_, dag_func) in &dag_module.functions {
                for id in &dag_func.dag_basic_blocks {
                    let bb = &dag_func.dag_basic_block_arena[*id];
                    println!("{}: {:?}", id.index(), bb);
                }
            }
        );

        let mut machine_module = dag::mc_convert::MIConverter::new().convert_module(dag_module);

        machine::phi_elimination::PhiElimination::new().run_on_module(&mut machine_module);
        machine::two_addr::TwoAddressConverter::new().run_on_module(&mut machine_module);
        machine::regalloc::RegisterAllocator::new().run_on_module(&mut machine_module);
        machine::pro_epi_inserter::PrologueEpilogueInserter::new()
            .run_on_module(&mut machine_module);

        debug!(
            println!("MachineModule dump:");
            for (_, machine_func) in &machine_module.functions {
                let mut idx = 0;
                println!("Machine function '{}':", machine_func.name);
                for bb_id in &machine_func.basic_blocks {
                    let bb = &machine_func.basic_block_arena[*bb_id];
                    println!("Machine basic block: {:?}", bb);
                    for instr in &*bb.iseq_ref() {
                        println!("{}: {:?}", idx, machine_func.instr_arena[*instr]);
                        idx += 1;
                    }
                    println!()
                }
            }
        );

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
            function_map: FxHashMap::default(),
            bb_to_label: FxHashMap::default(),
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
        let f_entry = self.get_function_entry_label(id);
        let entry = self.asm.offset();

        for (idx, arg) in args.iter().enumerate() {
            match arg {
                GenericValue::Int32(i) => dynasm!(self.asm; mov Ra(phys_reg_to_dynasm_reg(
                    RegisterClassKind::GR32.get_nth_arg_reg(idx).unwrap())), *i),
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

        match &module.function_ref(id).ty.get_function_ty().unwrap().ret_ty {
            Type::Int32 => GenericValue::Int32(f() as i32),
            Type::Void => {
                f();
                GenericValue::None
            }
            _ => unimplemented!(),
        }
    }

    pub fn compile_module(&mut self, module: &MachineModule) {
        for (f_id, _) in &module.functions {
            self.compile_function(module, f_id);
        }
    }

    fn compile_function(&mut self, module: &MachineModule, id: MachineFunctionId) {
        self.bb_to_label.clear();

        let f = module.function_ref(id);

        if f.internal {
            return;
        }

        let f_entry = self.get_function_entry_label(id);

        let frame_objects = FrameObjectsInfo::new(f);

        // prologue
        // TODO: Create PrologueInserter for machine code

        dynasm!(self.asm
            ; => f_entry
            // ; push rbp
            // ; mov rbp, rsp
            // ; sub rsp, roundup(frame_objects.total_size() + /*push rbp=*/8, 16) - 8
        );

        for bb_id in &f.basic_blocks {
            let bb = &f.basic_block_arena[*bb_id];

            if bb_id.index() != 0 {
                let label = self.get_label_of_bb(*bb_id);
                dynasm!(self.asm; =>label);
            }

            for instr in &*bb.iseq_ref() {
                let instr = &f.instr_arena[*instr];
                match instr.opcode {
                    MachineOpcode::MOVri32 => self.compile_mov_ri32(instr),
                    MachineOpcode::MOVrr32 => self.compile_mov_rr32(instr),
                    MachineOpcode::MOVri64 => self.compile_mov_ri64(instr),
                    MachineOpcode::MOVrr64 => self.compile_mov_rr64(instr),
                    MachineOpcode::MOVrm32 => self.compile_mov_rm32(&frame_objects, instr),
                    MachineOpcode::MOVrm64 => self.compile_mov_rm64(&frame_objects, instr),
                    MachineOpcode::MOVmr32 => self.compile_mov_mr32(&frame_objects, instr),
                    MachineOpcode::MOVmi32 => self.compile_mov_mi32(&frame_objects, instr),
                    MachineOpcode::LEA64 => self.compile_lea64(&frame_objects, instr),
                    MachineOpcode::RET => self.compile_ret(),
                    MachineOpcode::PUSH64 => self.compile_push64(instr),
                    MachineOpcode::POP64 => self.compile_pop64(instr),
                    MachineOpcode::Add => self.compile_add(&frame_objects, instr),
                    MachineOpcode::ADDrr32 => self.compile_add_rr32(instr),
                    MachineOpcode::ADDri32 => self.compile_add_ri32(instr),
                    MachineOpcode::SUBrr32 => self.compile_sub_rr32(instr),
                    MachineOpcode::SUBri32 => self.compile_sub_ri32(instr),
                    MachineOpcode::SUBr64i32 => self.compile_sub_r64i32(instr),
                    MachineOpcode::IMULrr32 => self.compile_imul_rr32(instr),
                    MachineOpcode::IMULrri32 => self.compile_imul_rri32(instr),
                    MachineOpcode::IDIV => self.compile_idiv(&frame_objects, instr),
                    MachineOpcode::CDQ => self.compile_cdq(&frame_objects, instr),
                    MachineOpcode::MOVrmi32 => self.compile_mov_rmi32(&frame_objects, instr),
                    MachineOpcode::MOVrmri32 => self.compile_mov_rmri32(&frame_objects, instr),
                    MachineOpcode::MOVrrri32 => self.compile_mov_rrri32(&frame_objects, instr),
                    MachineOpcode::MOVmi32r32 => self.compile_mov_mi32r32(&frame_objects, instr),
                    MachineOpcode::MOVmi32i32 => self.compile_mov_mi32i32(&frame_objects, instr),
                    MachineOpcode::StoreFiOff => self.compile_store_fi_off(&frame_objects, instr),
                    MachineOpcode::StoreRegOff => self.compile_store_reg_off(&frame_objects, instr),
                    MachineOpcode::Call => self.compile_call(module, &frame_objects, instr),
                    MachineOpcode::Copy => self.compile_copy(instr),
                    MachineOpcode::BrccEq | MachineOpcode::BrccLe | MachineOpcode::BrccLt => {
                        self.compile_brcc(instr)
                    }
                    MachineOpcode::Br => self.compile_br(instr),
                    MachineOpcode::Ret => self.compile_return(&frame_objects, instr),
                    op => unimplemented!("{:?}", op),
                }
            }
        }
    }

    fn compile_mov_ri32(&mut self, instr: &MachineInstr) {
        assert!(matches!(instr.operand[0], MachineOperand::Constant(_)));
        assert!(matches!(
            instr.operand[0].as_constant(),
            MachineConstant::Int32(_)
        ));
        let r = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let i = instr.operand[0].as_constant().as_i32();
        dynasm!(self.asm; mov Rd(r), i);
    }

    fn compile_mov_rr32(&mut self, instr: &MachineInstr) {
        assert!(matches!(instr.operand[0], MachineOperand::Register(_)));
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(instr.operand[0].as_register().get_reg().unwrap());
        self.reg_copy(RegisterClassKind::GR32, r0, r1);
    }

    fn compile_mov_ri64(&mut self, _instr: &MachineInstr) {
        unimplemented!()
        // assert!(matches!(instr.operand[0], MachineOperand::Constant(_)));
        // assert!(matches!(
        //     instr.operand[0].as_constant(),
        //     MachineConstant::Int64(_)
        // ));
        //
        // let r = instr.def[0].get_reg().unwrap().get() as u8;
        // let i = instr.operand[0].as_constant().as_i64();
        // dynasm!(self.asm; mov Ra(r), i);
    }

    fn compile_mov_rr64(&mut self, instr: &MachineInstr) {
        assert!(matches!(instr.operand[0], MachineOperand::Register(_)));
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(instr.operand[0].as_register().get_reg().unwrap());
        self.reg_copy(RegisterClassKind::GR64, r0, r1);
    }

    fn compile_mov_rm32(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let m1 = instr.operand[0].as_frame_index();
        dynasm!(self.asm; mov Rd(r0), [rbp - fo.offset(m1.idx).unwrap()]);
    }

    fn compile_mov_rm64(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let m1 = instr.operand[0].as_frame_index();
        dynasm!(self.asm; mov Rd(r0), [rbp - fo.offset(m1.idx).unwrap()]);
    }

    fn compile_mov_mr32(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let m0 = instr.operand[0].as_frame_index();
        let r1 = phys_reg_to_dynasm_reg(instr.operand[1].as_register().get_reg().unwrap());
        dynasm!(self.asm; mov DWORD [rbp - fo.offset(m0.idx).unwrap()], Rd(r1));
    }

    fn compile_mov_mi32(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let m0 = instr.operand[0].as_frame_index();
        let i1 = instr.operand[1].as_constant().as_i32();
        dynasm!(self.asm; mov DWORD [rbp - fo.offset(m0.idx).unwrap()], i1);
    }

    fn compile_lea64(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let fi = instr.operand[0].as_frame_index();
        dynasm!(self.asm; lea Ra(r0), [rbp - fo.offset(fi.idx).unwrap()]);
    }

    fn compile_push64(&mut self, instr: &MachineInstr) {
        let op0 = &instr.operand[0];
        match op0 {
            MachineOperand::Register(reg) => dynasm!(self.asm; push Rq(register!(reg))),
            _ => unimplemented!(),
        }
    }

    fn compile_pop64(&mut self, instr: &MachineInstr) {
        let op0 = &instr.operand[0];
        match op0 {
            MachineOperand::Register(reg) => dynasm!(self.asm; pop Rq(register!(reg))),
            _ => unimplemented!(),
        }
    }

    fn compile_copy(&mut self, instr: &MachineInstr) {
        let rn = register!(instr);
        let op0 = &instr.operand[0];
        match op0 {
            MachineOperand::Register(reg) => self.reg_copy(reg.get_reg_class(), rn, register!(reg)),
            _ => unimplemented!(),
        }
    }

    fn compile_brcc(&mut self, instr: &MachineInstr) {
        let op0 = &instr.operand[0];
        let op1 = &instr.operand[1];
        let br = &instr.operand[2];
        match op0 {
            MachineOperand::Register(i0) => match op1 {
                MachineOperand::Constant(c) => match c {
                    MachineConstant::Int32(x) => dynasm!(self.asm; cmp Rd(register!(i0)), *x),
                    MachineConstant::Int64(_) => unimplemented!(),
                },
                MachineOperand::Register(i1) => match i0.get_reg_class() {
                    RegisterClassKind::GR32 => {
                        dynasm!(self.asm; cmp Rd(register!(i0)), Rd(register!(i1)))
                    }
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            },
            e => unimplemented!("{:?}", e),
        }
        match br {
            MachineOperand::Branch(bb) => {
                let l = self.get_label_of_bb(*bb);
                match instr.opcode {
                    MachineOpcode::BrccEq => dynasm!(self.asm; jz => l),
                    MachineOpcode::BrccLe => dynasm!(self.asm; jle => l),
                    MachineOpcode::BrccLt => dynasm!(self.asm; jl => l),
                    _ => unreachable!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn compile_mov_rmi32(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let rn = register!(instr);
        let fi = instr.operand[0].as_frame_index();
        let off = &instr.operand[1];
        match off {
            MachineOperand::Constant(MachineConstant::Int32(i)) => {
                match &instr.def[0].info_ref().reg_class {
                    RegisterClassKind::GR32 => {
                        dynasm!(self.asm; mov Rd(rn), [rbp - fo.offset(fi.idx).unwrap() + i])
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn compile_mov_rmri32(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let rn = register!(instr);
        let fi = instr.operand[0].as_frame_index();
        let off = register!(instr.operand[1].as_register());
        let align = instr.operand[2].as_constant();

        match &instr.def[0].info_ref().reg_class {
            RegisterClassKind::GR32 => match align {
                MachineConstant::Int32(4) => {
                    dynasm!(self.asm;
                    mov Rd(rn), DWORD [rbp + 4*Ra(off) - fo.offset(fi.idx).unwrap()]
                    );
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn compile_mov_rrri32(&mut self, _fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let rn = register!(instr);
        let base = register!(instr.operand[0].as_register());
        let off = register!(instr.operand[1].as_register());
        let align = instr.operand[2].as_constant();

        match &instr.def[0].info_ref().reg_class {
            RegisterClassKind::GR32 => match align {
                MachineConstant::Int32(4) => {
                    dynasm!(self.asm; mov Rd(rn), DWORD [Ra(base) + 4*Ra(off)]);
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn compile_mov_mi32r32(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let m0 = fo.offset(instr.operand[0].as_frame_index().idx).unwrap();
        let i1 = instr.operand[1].as_constant().as_i32();
        let r2 = phys_reg_to_dynasm_reg(instr.operand[2].as_register().get_reg().unwrap());
        dynasm!(self.asm; mov DWORD [rbp - m0 + i1], Rd(r2));
    }

    fn compile_mov_mi32i32(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let m0 = fo.offset(instr.operand[0].as_frame_index().idx).unwrap();
        let i1 = instr.operand[1].as_constant().as_i32();
        let i2 = instr.operand[2].as_constant().as_i32();
        dynasm!(self.asm; mov DWORD [rbp - m0 + i1], i2);
    }

    fn compile_store_fi_off(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let fi = fo.offset(instr.operand[0].as_frame_index().idx).unwrap();
        let off = instr.operand[1].as_register();
        let align = instr.operand[2].as_constant();
        let src = &instr.operand[3];
        match src {
            MachineOperand::Constant(MachineConstant::Int32(i)) => {
                assert_eq!(align, &MachineConstant::Int32(4));
                dynasm!(self.asm; mov DWORD [rbp + 4*Ra(register!(off)) - fi], *i)
            }
            MachineOperand::Register(r) => match align {
                MachineConstant::Int32(4) => {
                    dynasm!(self.asm; mov DWORD [rbp + 4*Ra(register!(off)) - fi], Rd(register!(r)))
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn compile_store_reg_off(&mut self, _fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let base = register!(instr.operand[0].as_register());
        let off = register!(instr.operand[1].as_register());
        let align = instr.operand[2].as_constant();
        let src = &instr.operand[3];
        match src {
            MachineOperand::Constant(MachineConstant::Int32(i)) => {
                assert_eq!(align, &MachineConstant::Int32(4));
                dynasm!(self.asm; mov DWORD [Ra(base) + 4*Ra(off)], *i)
            }
            MachineOperand::Register(r) => match align {
                MachineConstant::Int32(4) => {
                    dynasm!(self.asm; mov DWORD [Ra(base) + 4*Ra(off)], Rd(register!(r)))
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn compile_call(
        &mut self,
        module: &MachineModule,
        _fo: &FrameObjectsInfo,
        instr: &MachineInstr,
    ) {
        let callee_id = module
            .find_function_by_name(match &instr.operand[0] {
                MachineOperand::GlobalAddress(GlobalValueInfo::FunctionName(n)) => n.as_str(),
                _ => unimplemented!(),
            })
            .unwrap();
        let callee_entity = module.function_ref(callee_id);
        // let ret_ty = &callee_entity.ty.get_function_ty().unwrap().ret_ty;
        let rsp_offset = 0;

        // rsp_offset += self.push_args(f, args);
        self.assign_args_to_regs(&instr.operand[1..]);

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
            let f_entry = self.get_function_entry_label(callee_id);
            dynasm!(self.asm; call => f_entry);
        }

        // match ret_ty {
        //     Type::Int32 => self.reg_copy(ret_ty, register!(instr), 0),
        //     Type::Void => {}
        //     _ => unimplemented!(),
        // }

        // for save_reg in save_regs.iter().rev() {
        //     dynasm!(self.asm; pop Ra(*save_reg));
        // }
    }

    fn compile_add_rr32(&mut self, instr: &MachineInstr) {
        // instr.operand[0] must be the same as instr.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(instr.operand[1].as_register().get_reg().unwrap());
        dynasm!(self.asm; add Rd(r0), Rd(r1));
    }

    fn compile_add_ri32(&mut self, instr: &MachineInstr) {
        // instr.operand[0] must be the same as instr.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let i1 = instr.operand[1].as_constant().as_i32();
        dynasm!(self.asm; add Rd(r0), i1);
    }

    fn compile_sub_rr32(&mut self, instr: &MachineInstr) {
        // instr.operand[0] must be the same as instr.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(instr.operand[1].as_register().get_reg().unwrap());
        dynasm!(self.asm; sub Rd(r0), Rd(r1));
    }

    fn compile_sub_ri32(&mut self, instr: &MachineInstr) {
        // instr.operand[0] must be the same as instr.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let i1 = instr.operand[1].as_constant().as_i32();
        dynasm!(self.asm; sub Rd(r0), i1);
    }

    fn compile_sub_r64i32(&mut self, instr: &MachineInstr) {
        // instr.operand[0] must be the same as instr.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let i1 = instr.operand[1].as_constant().as_i32();
        dynasm!(self.asm; sub Rq(r0), i1);
    }

    fn compile_imul_rr32(&mut self, instr: &MachineInstr) {
        // instr.operand[0] must be the same as instr.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(instr.operand[1].as_register().get_reg().unwrap());
        dynasm!(self.asm; imul Rd(r0), Rd(r1))
    }

    fn compile_imul_rri32(&mut self, instr: &MachineInstr) {
        // instr.operand[0] must be the same as instr.def[0] (they're tied)
        let r0 = phys_reg_to_dynasm_reg(instr.def[0].get_reg().unwrap());
        let r1 = phys_reg_to_dynasm_reg(instr.operand[0].as_register().get_reg().unwrap());
        let i2 = instr.operand[1].as_constant().as_i32();
        dynasm!(self.asm; imul Rd(r0), Rd(r1), i2);
    }

    fn compile_add(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let rn = register!(instr);
        let op0 = &instr.operand[0];
        let op1 = &instr.operand[1];
        match op0 {
            MachineOperand::FrameIndex(fi) => match op1 {
                MachineOperand::Constant(c) => match c {
                    MachineConstant::Int32(i) => {
                        dynasm!(self.asm;
                            lea Ra(rn), [rbp + i - fo.offset(fi.idx).unwrap()]);
                    }
                    _ => unimplemented!(),
                },
                MachineOperand::Register(r) => {
                    dynasm!(self.asm;
                        lea Ra(rn), [rbp + Ra(register!(r)) - fo.offset(fi.idx).unwrap()]);
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn compile_cdq(&mut self, _fo: &FrameObjectsInfo, _instr: &MachineInstr) {
        dynasm!(self.asm; cdq)
    }

    fn compile_idiv(&mut self, _fo: &FrameObjectsInfo, instr: &MachineInstr) {
        let (r, r_ty) = {
            let reg = instr.operand[0].as_register();
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

    fn compile_br(&mut self, instr: &MachineInstr) {
        match &instr.operand[0] {
            MachineOperand::Branch(bb) => {
                let label = self.get_label_of_bb(*bb);
                dynasm!(self.asm; jmp =>label)
            }
            _ => unimplemented!(),
        }
    }

    fn compile_ret(&mut self) {
        dynasm!(self.asm; ret);
    }

    fn compile_return(&mut self, fo: &FrameObjectsInfo, instr: &MachineInstr) {
        match &instr.operand[0] {
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
        }
    }

    fn get_label_of_bb(&mut self, bb_id: MachineBasicBlockId) -> DynamicLabel {
        if let Some(label) = self.bb_to_label.get(&bb_id) {
            return *label;
        }

        let new_label = self.asm.new_dynamic_label();
        self.bb_to_label.insert(bb_id, new_label);
        new_label
    }

    fn get_function_entry_label(&mut self, f_id: MachineFunctionId) -> DynamicLabel {
        if self.function_map.contains_key(&f_id) {
            return *self.function_map.get(&f_id).unwrap();
        }

        let f_entry = self.asm.new_dynamic_label();
        self.function_map.insert(f_id, f_entry);
        f_entry
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
