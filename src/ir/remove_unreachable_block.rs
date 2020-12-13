use crate::ir::{function::Function, module::Module};
use rustc_hash::FxHashSet;

pub struct RemoveUnreachableBlock {}

pub struct RemoveUnreachableBlockOnFunction<'a> {
    func: &'a mut Function,
}

impl RemoveUnreachableBlock {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            RemoveUnreachableBlockOnFunction::new(func).run()
        }
    }
}

impl<'a> RemoveUnreachableBlockOnFunction<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self { func }
    }

    pub fn run(&mut self) {
        let mut blocks2remove = FxHashSet::default();
        for (block_id, block) in &self.func.basic_blocks.arena {
            if block.pred.len() == 0 {
                blocks2remove.insert(block_id);
            }
        }
        // Do not remove entry block
        if let [entry, ..] = self.func.basic_blocks.order.as_slice() {
            blocks2remove.remove(entry);
        }
        self.func.basic_blocks.remove_blocks(&blocks2remove);
    }
}
