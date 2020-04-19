use crate::ir::{function::Function, module::Module, types::Types};

pub struct Mem2Reg {}

impl Mem2Reg {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            self.run_on_function(&module.types, func)
        }
    }

    fn run_on_function(&mut self, _tys: &Types, func: &mut Function) {
        for _id in &func.basic_blocks {}
        // for bb_id in &func.dag_basic_blocks {
        //     let bb = &func.dag_basic_block_arena[*bb_id];
        //     self.run_on_node(tys, &mut func.dag_heap, bb.entry.unwrap());
        // }
    }
}
