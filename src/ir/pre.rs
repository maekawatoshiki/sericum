use crate::analysis::dom_tree::{DominatorTree, DominatorTreeConstructor};
use crate::ir::{
    basic_block::{BasicBlock, BasicBlockId},
    builder::IRBuilder,
    function::Function,
    module::Module,
    opcode::{InstOperand, Instruction, InstructionId, Opcode},
    value::{InstructionValue, Value},
};
use std::cell::RefCell;
// use crate::traits::basic_block::*;
use id_arena::Arena;
use rustc_hash::{FxHashMap, FxHashSet};

pub struct PartialRedundancyElimination {}

struct GlobalPartialRedundancyEliminationOnFunction<'a> {
    func: &'a mut Function,
    bb_avails: AvailsInBB,
    dom_frontiers: FxHashSet<BasicBlockId>,
    inst_to_incomings_and_must_insert: FxHashMap<
        InstructionId,
        (
            BasicBlockId,
            Vec<(InstructionId, BasicBlockId)>,
            Vec<BasicBlockId>,
        ),
    >,
    removal_list: Vec<InstructionId>,
}

impl PartialRedundancyElimination {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal || func.is_empty() {
                continue;
            }

            GlobalPartialRedundancyEliminationOnFunction {
                func,
                bb_avails: AvailsInBB::default(),
                dom_frontiers: FxHashSet::default(),
                inst_to_incomings_and_must_insert: FxHashMap::default(),
                removal_list: vec![],
            }
            .run()
        }
    }
}

type Subexprs = FxHashMap<Opcode, FxHashMap<InstOperand, InstructionId>>;
type AvailsInBB = FxHashMap<BasicBlockId, Subexprs>;

impl<'a> GlobalPartialRedundancyEliminationOnFunction<'a> {
    pub fn run_sub(
        &mut self,
        dom_tree: &DominatorTree<BasicBlock>,
        root: BasicBlockId,
        mut commons: Subexprs,
    ) {
        // fn find_common<'a>(
        //     commons: &'a Subexprs,
        //     arena: &mut Arena<Instruction>,
        //     inst_id: &InstructionId,
        // ) -> Option<&'a InstructionId> {
        //     let inst = &arena[*inst_id];
        //     let opcode = inst.opcode;
        //     commons
        //         .get(&opcode)
        //         .map_or(None, |map| map.get(&inst.operand))
        // };

        let bb = &self.func.basic_blocks.arena[root];
        for &inst_id in &*bb.iseq_ref() {
            if self.removal_list.contains(&inst_id) {
                continue;
            }
            // if let Some(common) = find_common(&commons, &mut self.func.inst_table, &inst_id) {
            //     let common_val = Value::Instruction(InstructionValue {
            //         id: *common,
            //         func_id: self.func.id.unwrap(),
            //     });
            //     Instruction::replace_all_uses(&mut self.func.inst_table, inst_id, common_val);
            //     self.removal_list.push(inst_id);
            //     continue;
            // }

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
                if inst.operand.args().iter().all(|id| {
                    if let Value::Instruction(v) = id {
                        self.func.inst_table[v.id].opcode != Opcode::Phi
                    } else {
                        true
                    }
                }) {
                    commons
                        .entry(inst.opcode)
                        .or_insert(FxHashMap::default())
                        .entry(inst.operand.clone())
                        .or_insert(inst_id);
                }
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
        ) -> Option<(Vec<(InstructionId, BasicBlockId)>, Vec<BasicBlockId>)> {
            let mut incomings = vec![];
            let mut must_insert = vec![];
            for pred in preds {
                let e = &avails[pred];
                if let Some(inst_id) = find_common(e, arena, inst_id) {
                    incomings.push((*inst_id, *pred));
                } else {
                    must_insert.push(*pred);
                }
            }
            if incomings.len() > 0 {
                Some((incomings, must_insert))
            } else {
                None
            }
        };

        for &inst_id in &*bb.iseq_ref() {
            if self.removal_list.contains(&inst_id) {
                continue;
            }
            if let Some((incomings, must_insert)) = is_common(
                &self.func.basic_block_ref(*ebb_start).pred,
                &self.bb_avails,
                &self.func.inst_table,
                &inst_id,
            ) {
                self.inst_to_incomings_and_must_insert
                    .entry(inst_id)
                    .or_insert((*ebb_start, incomings, must_insert));
            }
        }

        for succ in self.func.basic_block_ref(*root).succ.clone() {
            // Do not visit a dom frontier block twice
            if self.dom_frontiers.contains(&succ) {
                continue;
            }
            self.run_sub2(&succ, ebb_start);
        }
    }

    fn run_sub3(&mut self) {
        let mut done = FxHashSet::default();
        let mut phis = vec![];
        let mut splits = FxHashMap::default();
        for (inst, (ebb_start, incomings, must_inserts)) in &self.inst_to_incomings_and_must_insert
        {
            //must_insert->ebb_start
            //must_insert->split->ebb_start
            for must_insert in must_inserts {
                if !done.insert(*must_insert) {
                    continue;
                }
                let split = self.func.append_basic_block_before(*ebb_start);
                splits.insert(must_insert, split);
                {
                    let succs = &mut self.func.basic_blocks.arena[*must_insert].succ;
                    succs.retain(|p| p != ebb_start);
                    succs.insert(split);
                }
                {
                    let preds = &mut self.func.basic_blocks.arena[*ebb_start].pred;
                    preds.retain(|p| p != must_insert);
                }
                for &id in self.func.basic_blocks.arena[*must_insert]
                    .iseq_ref()
                    .iter()
                    .rev()
                {
                    if !self.func.inst_table[id].opcode.is_terminator() {
                        break;
                    }
                    Instruction::replace_block_operand(
                        &mut self.func.inst_table,
                        id,
                        ebb_start,
                        split,
                    )
                }
                for &id in self.func.basic_blocks.arena[*ebb_start]
                    .iseq_ref()
                    .iter()
                    .rev()
                {
                    if self.func.inst_table[id].opcode != Opcode::Phi {
                        continue;
                    }
                    Instruction::replace_block_operand(
                        &mut self.func.inst_table,
                        id,
                        must_insert,
                        split,
                    )
                }
                {
                    self.func.basic_blocks.arena[split]
                        .pred
                        .insert(*must_insert);
                    let mut builder = self.func.ir_builder();
                    builder.set_insert_point(split);
                    builder.build_br(*ebb_start);
                }
            }
            let mut args = vec![];
            let mut blocks = vec![];
            for (inst_id, block) in incomings {
                args.push(Value::Instruction(InstructionValue {
                    func_id: self.func.id.unwrap(),
                    id: *inst_id,
                }));
                blocks.push(*block);
            }
            for m in must_inserts {
                let split = splits[m];
                let mut inst = self.func.inst_table[*inst].clone();
                inst.users = RefCell::new(vec![]);
                inst.parent = split;
                inst.id = None;
                let id = self.func.alloc_inst(inst);
                {
                    let mut builder = self.func.ir_builder();
                    builder.set_insert_point_before_terminator(split);
                    builder.insert(id);
                }
                args.push(Value::Instruction(InstructionValue {
                    func_id: self.func.id.unwrap(),
                    id,
                }));
                blocks.push(split);
            }
            let ty = self.func.get_value_type(&args[0]);
            let phi = Instruction::new(
                Opcode::Phi,
                InstOperand::Phi { args, blocks },
                ty,
                *ebb_start,
            );
            phis.push((*ebb_start, *inst, phi));
        }

        let mut q = vec![];
        for (ebb_start, inst2replace, phi) in phis {
            let id = self.func.alloc_inst(phi);
            self.func.basic_blocks.arena[ebb_start]
                .iseq_ref_mut()
                .insert(0, id);
            let val = Value::Instruction(InstructionValue {
                func_id: self.func.id.unwrap(),
                id,
            });
            q.push((inst2replace, val));
        }
        for (inst2replace, val) in q {
            Instruction::replace_all_uses(&mut self.func.inst_table, inst2replace, val);
            self.removal_list.push(inst2replace);
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

        self.run_sub3();

        debug!(println!(
            "function '{}': {} insts removed",
            self.func.name,
            self.removal_list.len()
        ));

        for remove in self.removal_list {
            println!("REMOVE {:?}", remove.index());
            self.func.remove_inst(remove);
        }
    }
}
