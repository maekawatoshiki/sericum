use super::opcode::{Instruction, Opcode, Operand};
use crate::traits::basic_block::BasicBlocksTrait;
use rustc_hash::FxHashSet;

use super::{function::Function, module::Module};

pub struct BranchFolding {
    removed_blocks: usize,
}

impl BranchFolding {
    pub fn new() -> Self {
        Self { removed_blocks: 0 }
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, f) in &mut module.functions {
            if f.is_internal {
                continue;
            }

            self.removed_blocks = 0;
            self.remove_unreachable(f);
            self.remove_empty_block(f);
            self.merge_blocks(f);

            debug!(println!(
                "{}: removed {} blocks",
                f.name, self.removed_blocks
            ));
        }
    }

    fn remove_unreachable(&mut self, f: &mut Function) {
        let mut worklist = vec![];
        for (i, &id) in f.basic_blocks.get_order().iter().enumerate() {
            let block = &f.basic_blocks.get_arena()[id];
            if i == 0 {
                continue;
            }
            if block.pred.len() == 0 {
                worklist.push((id, block.succ.clone()));
            }
        }

        self.removed_blocks += worklist.len();

        for (bb, succs) in worklist {
            for succ in succs {
                f.basic_blocks.arena[succ].pred.remove(&bb);
            }
            f.basic_blocks.order.retain(|&b| b != bb);
        }
    }

    // bb1: (succ: bb2)
    //     a = b + c
    //     br bb2;
    // bb2: (pred: bb1)
    //     ret a
    //
    // converts into:
    //
    // bb2:
    //     a = b + c
    //     ret a
    fn merge_blocks(&mut self, f: &mut Function) {
        let mut worklist = f.basic_blocks.order.clone();

        loop {
            let mut blocks_to_merge = vec![];
            for &id in &worklist {
                let block = &f.basic_blocks.arena[id];

                if block.succ.len() == 0 {
                    continue;
                }

                let succ = *block.succ.iter().next().unwrap();
                let mergeable_into_succ = block.succ.len() == 1 && {
                    let succ_preds = &f.basic_blocks.get_arena()[succ].pred;
                    succ_preds.len() == 1 && *succ_preds.iter().next().unwrap() == id
                };

                if mergeable_into_succ {
                    blocks_to_merge.push((id, succ));
                }
            }

            if blocks_to_merge.len() == 0 {
                break;
            }

            let mut removed = FxHashSet::default();
            for &(block, succ) in &blocks_to_merge {
                if removed.contains(&block) {
                    continue;
                }

                let block_ = &f.basic_blocks.get_arena()[block];
                let mut insts_to_remove = vec![];
                for &val in block_.iseq_ref().iter().rev() {
                    let inst_id = val.as_instruction().id;
                    if f.inst_table[inst_id].opcode.is_terminator() {
                        insts_to_remove.push(inst_id);
                        continue;
                    }
                    break;
                }
                for id in insts_to_remove {
                    f.remove_inst(id);
                }

                for &val in &*f.basic_blocks.get_arena()[succ].iseq_ref() {
                    let inst_id = val.as_instruction().id;
                    f.inst_table[inst_id].parent = block;
                }
                let succ_succ = &f.basic_blocks.get_arena()[succ].succ;

                // succ's succ may have phi nodes. replace phi incoming blocks if necessary.
                for s in succ_succ {
                    for &val in &*f.basic_blocks.get_arena()[*s].iseq_ref() {
                        let inst_id = val.as_instruction().id;
                        Instruction::replace_operand(
                            &mut f.inst_table,
                            inst_id,
                            &Operand::BasicBlock(succ),
                            Operand::BasicBlock(block),
                        );
                    }
                }

                // merge succ into block
                f.basic_blocks.merge(&block, &succ);
                removed.insert(succ);
                self.removed_blocks += 1;
            }

            worklist = blocks_to_merge.into_iter().map(|(x, _)| x).collect();
        }
    }

    fn remove_empty_block(&mut self, f: &mut Function) {
        let mut worklist = vec![];

        for &id in &f.basic_blocks.order {
            let block = &f.basic_blocks.arena[id];

            if block.iseq_ref().len() != 1 {
                continue;
            }

            if block.succ.len() == 0 {
                continue;
            }

            let inst = &f.inst_table[block.iseq_ref()[0].as_instruction().id];
            let succ = *block.succ.iter().next().unwrap();
            let only_one_succ = block.succ.len() == 1;
            let succ_preds = &f.basic_blocks.get_arena()[succ].pred;
            let succ_pred = *succ_preds.iter().next().unwrap();
            let only_one_succ_pred = succ_preds.len() == 1 && succ_pred == id;
            let succ_has_no_phi = f.basic_blocks.get_arena()[succ]
                .iseq_ref()
                .iter()
                .all(|val| {
                    let id = val.as_instruction().id;
                    f.inst_table[id].opcode != Opcode::Phi
                });
            let removable = inst.opcode == Opcode::Br
                && only_one_succ
                && (only_one_succ_pred || succ_has_no_phi);

            if removable {
                worklist.push((id, *inst.operands[0].as_basic_block()));
            }
        }

        for &(block_to_remove, new_dst) in &worklist {
            let preds = f.basic_blocks.arena[block_to_remove].pred.clone();
            let succs = f.basic_blocks.arena[block_to_remove].succ.clone();

            for &bb in &preds {
                let cur = &mut f.basic_blocks.arena[bb];
                cur.succ.remove(&block_to_remove);
                cur.succ.insert(new_dst);
            }

            for &bb in &succs {
                let cur = &mut f.basic_blocks.arena[bb];
                cur.pred.remove(&block_to_remove);
                cur.pred = &cur.pred | &preds;
            }

            for &bb in &preds {
                let cur = &mut f.basic_blocks.arena[bb];
                for val in cur.iseq_ref().iter().rev() {
                    let inst_id = val.as_instruction().id;
                    Instruction::replace_operand(
                        &mut f.inst_table,
                        inst_id,
                        &Operand::BasicBlock(block_to_remove),
                        Operand::BasicBlock(new_dst),
                    );
                }
            }
        }

        self.removed_blocks += worklist.len();

        for (remove, _) in worklist {
            f.basic_blocks.order.retain(|&bb| bb != remove);
        }
    }
}
