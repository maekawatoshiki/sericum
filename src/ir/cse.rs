use crate::analysis::dom_tree::{DominatorTree, DominatorTreeConstructor};
use crate::ir::{
    basic_block::{BasicBlock, BasicBlockId},
    function::Function,
    module::Module,
    opcode::{InstOperand, Instruction, InstructionId, Opcode},
    value::{InstructionValue, Value},
};
// use crate::traits::basic_block::*;
use id_arena::Arena;
use rustc_hash::{FxHashMap, FxHashSet};

pub struct CommonSubexprElimination {}

struct GlobalCommonSubexprEliminationOnFunction<'a> {
    func: &'a mut Function,
    bb_avails: AvailsInBB,
    dom_frontiers: FxHashSet<BasicBlockId>,
    removal_list: Vec<InstructionId>,
}

impl CommonSubexprElimination {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal || func.is_empty() {
                continue;
            }

            GlobalCommonSubexprEliminationOnFunction {
                func,
                bb_avails: AvailsInBB::default(),
                dom_frontiers: FxHashSet::default(),
                removal_list: vec![],
            }
            .run()
        }
    }
}

type Subexprs = FxHashMap<Opcode, FxHashMap<InstOperand, InstructionId>>;
type AvailsInBB = FxHashMap<BasicBlockId, Subexprs>;

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
                .map_or(None, |map| map.get(&inst.operand))
        };

        let bb = &self.func.basic_blocks.arena[root];

        for &inst_id in &*bb.iseq_ref() {
            if self.removal_list.contains(&inst_id) {
                continue;
            }
            if let Some(common) = find_common(&commons, &mut self.func.inst_table, &inst_id) {
                let common_val = Value::Instruction(InstructionValue {
                    id: *common,
                    func_id: self.func.id.unwrap(),
                    ty: self.func.inst_table[*common].ty,
                });
                Instruction::replace_all_uses(&mut self.func.inst_table, inst_id, common_val);
                self.removal_list.push(inst_id);
                continue;
            }

            let inst = &self.func.inst_table[inst_id];
            if matches!(
                inst.opcode,
                Opcode::Add
                    | Opcode::Sub
                    | Opcode::Mul
                    | Opcode::Div
                    | Opcode::Rem
                    | Opcode::Shl
                    | Opcode::SIToFP
                    | Opcode::FPToSI
                    | Opcode::Sext
            ) {
                commons
                    .entry(inst.opcode)
                    .or_insert(FxHashMap::default())
                    .entry(inst.operand.clone())
                    .or_insert(inst_id);
            }
        }

        self.bb_avails.insert(root, commons.clone());

        for &frontier in dom_tree
            .dominance_frontier_of(root)
            .unwrap_or(&FxHashSet::default())
        {
            self.dom_frontiers.insert(frontier);
        }

        for &child in dom_tree.tree.get(&root).unwrap_or(&FxHashSet::default()) {
            self.run_sub(dom_tree, child, commons.clone())
        }
    }

    fn run_sub2(&mut self, root: &BasicBlockId, ebb_start: &BasicBlockId) {
        let bb = &self.func.basic_blocks.arena[*root];

        fn find_common<'a>(
            commons: &'a Subexprs,
            arena: &Arena<Instruction>,
            inst_id: &InstructionId,
        ) -> Option<&'a InstructionId> {
            let inst = &arena[*inst_id];
            let opcode = inst.opcode;
            commons
                .get(&opcode)
                .map_or(None, |map| map.get(&inst.operand))
        };
        fn is_common<'a>(
            preds: &FxHashSet<BasicBlockId>,
            avails: &AvailsInBB,
            arena: &Arena<Instruction>,
            inst_id: &InstructionId,
        ) -> Option<Vec<(InstructionId, BasicBlockId)>> {
            let mut incomings = vec![];
            for pred in preds {
                let e = &avails[pred];
                if let Some(inst_id) = find_common(e, arena, inst_id) {
                    incomings.push((*inst_id, *pred));
                } else {
                    break;
                }
            }
            if preds.len() == incomings.len() {
                Some(incomings)
            } else {
                None
            }
        };

        let mut phis = vec![];
        for &inst_id in &*bb.iseq_ref() {
            if self.removal_list.contains(&inst_id) {
                continue;
            }
            if let Some(incomings) = is_common(
                &self.func.basic_block_ref(*ebb_start).pred,
                &self.bb_avails,
                &self.func.inst_table,
                &inst_id,
            ) {
                let mut args = vec![];
                let mut blocks = vec![];
                for (inst_id, block) in incomings {
                    args.push(Value::Instruction(InstructionValue {
                        func_id: self.func.id.unwrap(),
                        id: inst_id,
                        ty: self.func.inst_table[inst_id].ty,
                    }));
                    blocks.push(block);
                }
                let ty = args[0].as_instruction().ty;
                let phi = Instruction::new(
                    Opcode::Phi,
                    InstOperand::Phi { args, blocks },
                    ty,
                    *ebb_start,
                );
                phis.push((inst_id, phi));
            }
        }

        for (inst2replace, phi) in phis {
            let ty = phi.ty;
            let id = self.func.alloc_inst(phi);
            self.func.basic_blocks.arena[*ebb_start]
                .iseq_ref_mut()
                .insert(0, id);
            let val = Value::Instruction(InstructionValue {
                func_id: self.func.id.unwrap(),
                id,
                ty,
            });
            Instruction::replace_all_uses(&mut self.func.inst_table, inst2replace, val);
            self.removal_list.push(inst2replace);
        }

        for succ in self.func.basic_block_ref(*root).succ.clone() {
            // Do not visit a dom frontier block twice
            if self.dom_frontiers.contains(&succ) {
                continue;
            }
            self.run_sub2(&succ, ebb_start);
        }
    }

    pub fn run(mut self) {
        let dom_tree = DominatorTreeConstructor::new(&self.func.basic_blocks).construct();

        self.run_sub(
            &dom_tree,
            self.func.basic_blocks.order[0],
            FxHashMap::default(),
        );

        for df in self.dom_frontiers.clone() {
            self.run_sub2(&df, &df)
        }

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
