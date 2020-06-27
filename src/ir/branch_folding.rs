use super::opcode::{Instruction, Opcode, Operand};
use crate::traits::basic_block::BasicBlocksTrait;
use rustc_hash::FxHashSet;

use super::{function::Function, module::Module};

// Must run after phi elimination
pub struct BranchFolding {}

// impl ModulePassTrait for BranchFolding {
//     type M = MachineModule;
//
//     fn name(&self) -> &'static str {
//         "BranchFolding"
//     }
//
//     fn run_on_module(&mut self, module: &mut Self::M) {
//         self.run_on_module(module)
//     }
// }

impl BranchFolding {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, f) in &mut module.functions {
            if f.is_internal {
                continue;
            }
            self.remove_unreachable(f);
            // self.remove_empty_block(f);
            self.merge_blocks(f);
        }
    }

    fn remove_unreachable(&mut self, f: &mut Function) {
        let mut worklist = vec![];
        let mut remove = vec![];
        for (i, &id) in f.basic_blocks.get_order().iter().enumerate() {
            let block = &f.basic_blocks.get_arena()[id];
            if i == 0 {
                continue;
            }
            if block.pred.len() == 0 {
                remove.push(id);
                for &succ in &block.succ {
                    worklist.push((id, succ));
                }
            }
        }
        for (bb, succ) in worklist {
            f.basic_blocks.arena[succ].pred.remove(&bb);
        }
        for bb in remove {
            f.basic_blocks.order.retain(|&b| b != bb);
        }
    }

    fn merge_blocks(&mut self, f: &mut Function) {
        loop {
            let mut blocks_to_merge = vec![];
            for &id in &f.basic_blocks.order {
                let block = &f.basic_blocks.arena[id];
                let mergeable_into_succ = block.succ.len() == 1 && {
                    let succ_preds =
                        &f.basic_blocks.get_arena()[*block.succ.iter().next().unwrap()].pred;
                    succ_preds.len() == 1 && *succ_preds.iter().next().unwrap() == id
                };

                if mergeable_into_succ {
                    blocks_to_merge.push(id);
                }
            }
            if blocks_to_merge.len() == 0 {
                break;
            }
            let mut removed = FxHashSet::default();
            for block in blocks_to_merge {
                if removed.contains(&block) {
                    continue;
                }
                let block_ = &f.basic_blocks.get_arena()[block];
                let mut remove = vec![];
                for &val in block_.iseq_ref().iter().rev() {
                    let inst_id = val.as_instruction().id;
                    if f.inst_table[inst_id].opcode.is_terminator() {
                        remove.push(inst_id);
                    } else {
                        break;
                    }
                }
                for id in remove {
                    f.remove_inst(id);
                }
                let succ = *f.basic_blocks.get_arena()[block]
                    .succ
                    .iter()
                    .next()
                    .unwrap();
                for &val in &*f.basic_blocks.get_arena()[succ].iseq_ref() {
                    let inst_id = val.as_instruction().id;
                    f.inst_table[inst_id].parent = block;
                }
                let succ_succ = &f.basic_blocks.get_arena()[succ].succ;
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
            }
        }
    }

    // Very simple branch folding. TODO: Implement further complicated one
    fn remove_empty_block(&mut self, f: &mut Function) {
        let mut worklist = vec![];

        for &id in &f.basic_blocks.order {
            let block = &f.basic_blocks.arena[id];
            // for (id, block) in f.basic_blocks.id_and_block() {
            if block.iseq_ref().len() > 1 {
                continue;
            }
            let inst = &f.inst_table[block.iseq_ref()[0].as_instruction().id];
            if inst.opcode == Opcode::Br {
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

        debug!(println!("{} blocks removed", worklist.len()));

        for (remove, _) in worklist {
            f.basic_blocks.order.retain(|&bb| bb != remove);
        }
    }
}
