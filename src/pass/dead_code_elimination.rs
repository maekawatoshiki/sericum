use super::Pass;
use crate::ir::{function::*, module::*, opcode::*};

pub struct DeadCodeEliminationPass {}

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &Module) {
        for (_, f) in &module.functions {
            self.run_on_function(&f);
        }
    }

    pub fn run_on_function(&mut self, f: &Function) {
        for (_, bb) in &f.basic_blocks {
            bb.iseq_ref_mut().drain_filter(|instr_val| {
                let instr = &f.instr_table[instr_val.get_instr_id().unwrap()];
                instr.can_be_eliminated()
            });
        }
    }
}

impl Pass for DeadCodeEliminationPass {
    fn run(&mut self, module: &Module) {
        self.run_on_module(module)
    }
}
