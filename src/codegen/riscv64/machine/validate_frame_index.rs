use super::super::{frame_object::*, machine::register::*};
use super::inst::*;
use crate::codegen::common::machine::{builder::*, function::*, module::MachineModule};
use crate::{ir::types::*, traits::pass::ModulePassTrait};

pub struct ValidateFrameIndex {}

impl ModulePassTrait for ValidateFrameIndex {
    type M = MachineModule;

    fn name(&self) -> &'static str {
        "ValidateFrameIndex"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module)
    }
}

impl ValidateFrameIndex {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
                continue;
            }
            self.run_on_function(&module.types, func);
        }
    }

    // TODO: Refine
    pub fn run_on_function(&mut self, tys: &Types, f: &mut MachineFunction) {
        let mut frame_objects = FrameObjectsInfo::new(tys, f);
        frame_objects.callee_saved_regs_byte += 8; /*s1*/

        let s0 = f.regs_info.get_phys_reg(GPR::S0);
        let s1 = f.regs_info.get_phys_reg(GPR::S1);
        let mut mem = vec![];
        let mut add = vec![];

        for (_, block) in f.body.basic_blocks.id_and_block() {
            for &inst_id in &*block.iseq_ref() {
                let inst = &mut f.body.inst_arena[inst_id];
                for operand in &mut inst.operand {
                    match operand {
                        MachineOperand::Mem(MachineMemOperand::FiReg(fi, _))
                            if !bits_within(frame_objects.offset(fi.idx).unwrap(), 12) =>
                        {
                            mem.push((inst_id, *fi));
                            block.liveness_ref_mut().add_phys_def(s1.as_phys_reg());
                            *operand = MachineOperand::Mem(MachineMemOperand::ImmReg(0, s1));
                        }
                        MachineOperand::FrameIndex(fi)
                            if !bits_within(frame_objects.offset(fi.idx).unwrap(), 12) =>
                        {
                            add.push((inst_id, *fi));
                            block.liveness_ref_mut().add_phys_def(s1.as_phys_reg());
                            *operand = MachineOperand::Register(s1);
                            assert!(inst.opcode == MachineOpcode::ADDI);
                            inst.opcode = MachineOpcode::ADD;
                        }
                        _ => {}
                    }
                }
            }
        }

        for (inst_id, fi) in add {
            let mut builder = Builder::new(f);
            builder.set_insert_point_before_inst(inst_id);
            let li = MachineInst::new_simple(
                MachineOpcode::LI,
                vec![MachineOperand::FrameIndex(fi)],
                builder.get_cur_bb().unwrap(),
            )
            .with_def(vec![s1]);
            builder.insert(li);
        }

        for (inst_id, fi) in mem {
            let mut builder = Builder::new(f);
            builder.set_insert_point_before_inst(inst_id);
            let li = MachineInst::new_simple(
                MachineOpcode::LI,
                vec![MachineOperand::FrameIndex(fi)],
                builder.get_cur_bb().unwrap(),
            )
            .with_def(vec![s1]);
            let add = MachineInst::new_simple(
                MachineOpcode::ADD,
                vec![MachineOperand::Register(s0), MachineOperand::Register(s1)],
                builder.get_cur_bb().unwrap(),
            )
            .with_def(vec![s1]);
            builder.insert(li);
            builder.insert(add);
        }
    }
}

fn bits_within(x: i32, y: u32) -> bool {
    (x << (32 - y)) >> (32 - y) == x
}
