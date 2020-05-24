use crate::ir::{function::Function, module::Module};

pub struct CommonSubexprElimination {}

struct CommonSubexprEliminationOnFunction<'a> {
    func: &'a mut Function,
    // inst_indexes: InstructionIndexes,
    // dom_tree: DominatorTree<BasicBlock>,
    // phi_block_to_allocas: FxHashMap<BasicBlockId, Vec<InstructionId>>,
}

impl CommonSubexprElimination {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
                continue;
            }

            // Mem2RegOnFunction {
            //     dom_tree: DominatorTreeConstructor::new(func.get_basic_blocks()).construct(),
            //     cur_func: func,
            //     inst_indexes: InstructionIndexes::new(),
            //     phi_block_to_allocas: FxHashMap::default(),
            // }
            // .run(&module.types);
        }
    }
}

impl<'a> CommonSubexprEliminationOnFunction<'a> {
    fn run(&mut self) {}
}
