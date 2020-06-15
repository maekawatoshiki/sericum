use crate::analysis::dom_tree::{DominatorTree, DominatorTreeConstructor};
use crate::ir::{
    basic_block::{BasicBlock, BasicBlockId, BasicBlocks},
    function::Function,
    module::Module,
    opcode::{Instruction, InstructionId, Opcode, Operand},
    value::{InstructionValue, Value},
};
// use crate::traits::basic_block::*;
use id_arena::Arena;
use rustc_hash::{FxHashMap, FxHashSet};

pub struct CommonSubexprElimination {}

struct LocalCommonSubexprEliminationOnFunction<'a> {
    func: &'a mut Function,
    dom_tree: DominatorTree<BasicBlock>,
    removal_list: Vec<InstructionId>,
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

            LocalCommonSubexprEliminationOnFunction {
                dom_tree: DominatorTreeConstructor::new(&func.basic_blocks).construct(),
                func,
                removal_list: vec![],
            }
            .run()
        }
    }
}

type C = FxHashMap<Opcode, FxHashMap<Vec<Operand>, InstructionId>>;

impl<'a> LocalCommonSubexprEliminationOnFunction<'a> {
    fn get_ordered_blocks(&self, root: BasicBlockId) -> Vec<BasicBlockId> {
        let mut blocks = vec![];
        blocks.push(root);
        for children in self.dom_tree.tree.get(&root) {
            for &child in children {
                blocks.append(&mut self.get_ordered_blocks(child));
            }
        }
        blocks
    }

    pub fn run_sub(&mut self, root: BasicBlockId, mut commons: C) {
        fn find_common<'a>(
            commons: &'a C,
            arena: &mut Arena<Instruction>,
            inst_id: &InstructionId,
        ) -> Option<&'a InstructionId> {
            let inst = &arena[*inst_id];
            let opcode = inst.opcode;
            commons
                .get(&opcode)
                .map_or(None, |map| map.get(&inst.operands))
        };

        let bb = &self.func.basic_blocks.arena[root];

        for inst_id in bb.iseq.borrow().iter().map(|v| v.as_instruction().id) {
            if self.removal_list.contains(&inst_id) {
                continue;
            }
            if let Some(common) = find_common(&commons, &mut self.func.inst_table, &inst_id) {
                let common_val = Value::Instruction(InstructionValue {
                    id: *common,
                    func_id: self.func.id.unwrap(),
                    ty: self.func.inst_table[*common].ty,
                });
                Instruction::replace_all_uses(
                    &mut self.func.inst_table,
                    inst_id,
                    Operand::Value(common_val),
                );
                self.removal_list.push(inst_id);
                continue;
            }

            let inst = &self.func.inst_table[inst_id];
            if matches!(
                inst.opcode,
                Opcode::GetElementPtr
                    | Opcode::Add
                    | Opcode::Sub
                    | Opcode::Mul
                    | Opcode::Div
                    | Opcode::Rem // | Opcode::Phi
            ) {
                commons
                    .entry(inst.opcode)
                    .or_insert(FxHashMap::default())
                    .entry(inst.operands.clone())
                    .or_insert(inst_id);
            }
        }

        let children = self
            .dom_tree
            .tree
            .get(&root)
            .unwrap_or(&FxHashSet::default())
            .clone();
        for child in children {
            self.run_sub(child, commons.clone())
        }
    }

    pub fn run(&mut self) {
        self.run_sub(self.func.basic_blocks.order[0], FxHashMap::default());
        debug!(println!(
            "{}: {} insts removed",
            self.func.name,
            self.removal_list.len()
        ));

        for &remove in &self.removal_list {
            self.func.remove_inst(remove);
        }
    }
}
