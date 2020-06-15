use crate::analysis::dom_tree::{DominatorTree, DominatorTreeConstructor};
use crate::ir::{
    basic_block::{BasicBlock, BasicBlockId},
    function::Function,
    module::Module,
    opcode::{Instruction, InstructionId, Opcode, Operand},
    value::{InstructionValue, Value},
};
// use crate::traits::basic_block::*;
use id_arena::Arena;
use rustc_hash::{FxHashMap, FxHashSet};

pub struct CommonSubexprElimination {}

struct GlobalCommonSubexprEliminationOnFunction<'a> {
    func: &'a mut Function,
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

            GlobalCommonSubexprEliminationOnFunction {
                func,
                removal_list: vec![],
            }
            .run()
        }
    }
}

type Subexprs = FxHashMap<Opcode, FxHashMap<Vec<Operand>, InstructionId>>;

impl<'a> GlobalCommonSubexprEliminationOnFunction<'a> {
    pub fn run_sub(
        &mut self,
        dom_tree: &DominatorTree<BasicBlock>,
        root: BasicBlockId,
        mut commons: Subexprs,
    ) {
        fn find_common<'a>(
            commons: &'a Subexprs,
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

        for &child in dom_tree.tree.get(&root).unwrap_or(&FxHashSet::default()) {
            self.run_sub(dom_tree, child, commons.clone())
        }
    }

    pub fn run(mut self) {
        let dom_tree = DominatorTreeConstructor::new(&self.func.basic_blocks).construct();

        self.run_sub(
            &dom_tree,
            self.func.basic_blocks.order[0],
            FxHashMap::default(),
        );

        debug!(println!(
            "function '{}': {} insts removed",
            self.func.name,
            self.removal_list.len()
        ));

        for remove in self.removal_list {
            self.func.remove_inst(remove);
        }
    }
}
