use crate::codegen::common::machine::{
    builder::*, function::MachineFunction, inst::*, module::MachineModule,
};
use crate::traits::basic_block::BasicBlocksTrait;
use crate::traits::pass::ModulePassTrait;
use rustc_hash::FxHashSet;

// Must run after phi elimination
pub struct BranchFolding {}

impl ModulePassTrait for BranchFolding {
    type M = MachineModule;

    fn name(&self) -> &'static str {
        "BranchFolding"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module)
    }
}

impl BranchFolding {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            if f.is_internal {
                continue;
            }

            self.remove_unreachable(f);
            self.remove_empty_block(f);
            self.merge_blocks(f);
            self.remove_jmp(f);
        }
    }

    fn remove_unreachable(&mut self, f: &mut MachineFunction) {
        let mut worklist = vec![];
        let mut remove = vec![];
        for (i, &id) in f.body.basic_blocks.get_order().iter().enumerate() {
            let block = &f.body.basic_blocks.get_arena()[id];
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
            f.body.basic_blocks.arena[succ].pred.remove(&bb);
        }
        for bb in remove {
            f.body.basic_blocks.order.retain(|&b| b != bb);
        }
    }

    fn merge_blocks(&mut self, f: &mut MachineFunction) {
        let mut worklist = f.body.basic_blocks.order.clone();

        loop {
            let mut blocks_to_merge = vec![];
            for &id in &worklist {
                let block = &f.body.basic_blocks.arena[id];
                let mergeable_into_succ = block.succ.len() == 1 && {
                    let succ_preds =
                        &f.body.basic_blocks.get_arena()[*block.succ.iter().next().unwrap()].pred;
                    succ_preds.len() == 1 && *succ_preds.iter().next().unwrap() == id
                };

                if mergeable_into_succ {
                    blocks_to_merge.push(id);
                }
            }
            if f.body.basic_blocks.order.len() <= 1 {
                break;
            }
            if blocks_to_merge.len() == 0 {
                break;
            }
            let mut removed = FxHashSet::default();
            for &block in &blocks_to_merge {
                if removed.contains(&block) {
                    continue;
                }
                let block_ = &f.body.basic_blocks.get_arena()[block];
                let mut remove = vec![];
                for &inst_id in block_.iseq_ref().iter().rev() {
                    if !f.body.inst_arena[inst_id].opcode.is_terminator() {
                        break;
                    }
                    remove.push(inst_id);
                }
                for id in remove {
                    f.remove_inst(id);
                }
                let succ = *f.body.basic_blocks.get_arena()[block]
                    .succ
                    .iter()
                    .next()
                    .unwrap();
                for &inst_id in &*f.body.basic_blocks.get_arena()[succ].iseq_ref() {
                    f.body.inst_arena[inst_id].parent = block;
                }
                // merge succ into block
                f.body.basic_blocks.merge(&block, &succ);
                removed.insert(succ);
            }
            worklist = blocks_to_merge;
        }
    }

    // Very simple branch folding. TODO: Implement further complicated one
    fn remove_empty_block(&mut self, f: &mut MachineFunction) {
        let mut worklist = vec![];

        for (id, block) in f.body.basic_blocks.id_and_block() {
            let v: &Vec<_> = &*block.iseq_ref();
            if v.len() > 1 {
                continue;
            }
            let inst = &f.body.inst_arena[block.iseq_ref()[0]];
            if inst.opcode.is_unconditional_jmp() && inst.operand[0].is_basic_block() {
                let b = inst.operand[0].as_basic_block();
                if f.body.basic_blocks.arena[id].pred.len() > 1 {
                    continue;
                }
                worklist.push((id, b));
            }
        }

        for &(block_to_remove, new_dst) in &worklist {
            let preds = f.body.basic_blocks.arena[block_to_remove].pred.clone();
            let succs = f.body.basic_blocks.arena[block_to_remove].succ.clone();

            for &bb in &preds {
                let cur = &mut f.body.basic_blocks.arena[bb];
                cur.succ.remove(&block_to_remove);
                cur.succ.insert(new_dst);
            }

            for &bb in &succs {
                let cur = &mut f.body.basic_blocks.arena[bb];
                cur.pred.remove(&block_to_remove);
                cur.pred = &cur.pred | &preds;
            }

            for &bb in &preds {
                let cur = &mut f.body.basic_blocks.arena[bb];
                for inst_id in cur.iseq_ref().iter().rev() {
                    let inst = &mut f.body.inst_arena[*inst_id];
                    inst.replace_operand_block(block_to_remove, new_dst);
                }
            }
        }

        debug!(println!("{} empty blocks removed", worklist.len(),));

        for (remove, _) in worklist {
            f.body.basic_blocks.order.retain(|&bb| bb != remove);
        }
    }

    fn remove_jmp(&mut self, f: &mut MachineFunction) {
        let mut last_jmp = None;
        let mut remove_list = vec![];
        let mut new_jmps = vec![];

        // before:
        //
        // L1:
        //  jeq L2
        //  jmp L3
        // L2:
        //  ...
        //  jmp L3
        // L3:
        //  ...
        //
        // after:
        //
        // L1:
        //  jne L3
        // L2:
        //  ...
        // L3:
        //  ...

        let order = &f.body.basic_blocks.order;
        for (i, &bb_id) in order.iter().enumerate() {
            let block = &f.body.basic_blocks.arena[bb_id];
            for &id in block.iseq_ref().iter().rev() {
                let inst = &f.body.inst_arena[id];

                if !inst.opcode.is_jmp() {
                    if let Some(jmp) = last_jmp.take() {
                        if let Some(dst) = f.body.inst_arena[jmp].get_jmp_dst() {
                            // If we jump to the next block, remove the jump instruction. It's
                            // unnecessary.
                            if i + 1 < order.len() && dst == order[i + 1] {
                                remove_list.push(jmp);
                            }
                        }
                    }
                    break;
                }

                if inst.opcode.is_conditional_jmp() {
                    let dst = inst.get_jmp_dst().unwrap();
                    // If we jump to the next block conditionally, flip the jump condition.
                    if i + 1 < order.len() && dst == order[i + 1] {
                        let opcode = inst.opcode.flip_conditional_jmp().unwrap();
                        let last_jmp = last_jmp.take().unwrap(); // there must be unconditional jump before conditional jump.
                        let new_dst = f.body.inst_arena[last_jmp].get_jmp_dst().unwrap();
                        // replace jump destination
                        let new_operands = inst
                            .operand
                            .clone()
                            .into_iter()
                            .map(|op| match op {
                                MachineOperand::Branch(_) => MachineOperand::Branch(new_dst),
                                _ => op,
                            })
                            .collect();
                        new_jmps.push(MachineInst::new_simple(opcode, new_operands, bb_id));
                        remove_list.push(last_jmp);
                        remove_list.push(id);
                    }
                    continue;
                }

                if inst.opcode.is_unconditional_jmp() {
                    last_jmp = Some(id);
                    continue;
                }
            }
        }

        for id in remove_list {
            f.remove_inst(id)
        }

        for jmp in new_jmps {
            let mut builder = Builder::new(f);
            builder.set_insert_point_at_end(jmp.parent);
            builder.insert(jmp);
        }
    }
}
