use super::Pass;
use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*};

pub struct DeadCodeEliminationPass<'a> {
    module: &'a Module,
}

impl<'a> DeadCodeEliminationPass<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    pub fn run_on_module(&mut self) {
        for (_, f) in &self.module.functions {
            self.run_on_function(&f);
        }
    }

    pub fn run_on_function(&self, f: &Function) {
        for (_, bb) in &f.basic_blocks {
            bb.iseq_ref_mut().drain_filter(|instr_val| {
                let instr = &f.instr_table[instr_val.get_instr_id().unwrap()];
                println!("{:?} - {}", instr, instr.can_be_eliminated());
                instr.can_be_eliminated()
            });
        }
    }
}

impl<'a> Pass for DeadCodeEliminationPass<'a> {
    fn run(module: &Module) {
        DeadCodeEliminationPass::new(module).run_on_module()
    }
}
