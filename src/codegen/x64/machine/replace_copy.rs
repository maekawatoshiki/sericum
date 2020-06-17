use super::super::dag::mc_convert::mov_rx;
use super::inst::MachineOpcode;
use crate::codegen::common::machine::{function::MachineFunction, module::MachineModule};
use crate::{ir::types::Types, traits::pass::ModulePassTrait};

pub struct ReplaceCopyWithProperMInst {}

impl ModulePassTrait for ReplaceCopyWithProperMInst {
    type M = MachineModule;

    fn name(&self) -> &'static str {
        "ReplaceCopy"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module)
    }
}

impl ReplaceCopyWithProperMInst {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            if f.is_internal {
                continue;
            }
            self.run_on_function(&module.types, f);
        }
    }

    pub fn run_on_function(&mut self, tys: &Types, f: &mut MachineFunction) {
        for (_, bb) in f.body.basic_blocks.id_and_block() {
            for inst_id in &*bb.iseq_ref() {
                let inst = &mut f.body.inst_arena[*inst_id];

                if inst.opcode != MachineOpcode::Copy {
                    continue;
                }

                let mov = mov_rx(tys, &f.regs_info, &inst.operand[0]).unwrap();
                inst.opcode = mov;
            }
        }
    }
}
