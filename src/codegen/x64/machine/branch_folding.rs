use super::inst::MachineOpcode;
use crate::codegen::common::machine::{function::MachineFunction, module::MachineModule};
use crate::traits::pass::ModulePassTrait;

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
            self.run_on_function(f);
        }
    }

    // Very simple branch folding. TODO: Implement further complicated one
    pub fn run_on_function(&mut self, f: &mut MachineFunction) {
        let mut worklist = vec![];

        for (id, block) in f.body.basic_blocks.id_and_block() {
            if block.iseq_ref().len() != 1 {
                continue;
            }
            let inst = &f.body.inst_arena[block.iseq_ref()[0]];
            if inst.opcode == MachineOpcode::JMP {
                worklist.push((id, inst.operand[0].as_basic_block()));
            }
        }

        for &(block_to_remove, new_dst) in &worklist {
            let preds = f.body.basic_blocks.arena[block_to_remove].pred.clone();
            let succs = f.body.basic_blocks.arena[block_to_remove].succ.clone();

            for &bb in &preds {
                let cur = &mut f.body.basic_blocks.arena[bb];
                cur.succ.remove_item(&block_to_remove).unwrap();
                cur.succ.push(new_dst);
                if cur.pred.remove_item(&block_to_remove).is_some() {
                    cur.pred.append(&mut preds.clone());
                }
            }

            for &bb in &succs {
                let cur = &mut f.body.basic_blocks.arena[bb];
                cur.pred.remove_item(&block_to_remove).unwrap();
                cur.pred.append(&mut preds.clone());
                if cur.succ.remove_item(&block_to_remove).is_some() {
                    cur.succ.push(new_dst);
                }
            }

            for &bb in &preds {
                let cur = &mut f.body.basic_blocks.arena[bb];
                for inst_id in cur.iseq_ref().iter().rev() {
                    let inst = &mut f.body.inst_arena[*inst_id];
                    inst.replace_operand_block(block_to_remove, new_dst);
                }
            }

            for &bb in &succs {
                let cur = &mut f.body.basic_blocks.arena[bb];
                for inst_id in cur.iseq_ref().iter().rev() {
                    let inst = &mut f.body.inst_arena[*inst_id];
                    assert!(preds.len() <= 1);
                    if preds.len() == 0 {
                        continue;
                    }
                    inst.replace_operand_block(block_to_remove, preds[0]);
                }
            }
        }

        debug!(println!("{} blocks removed", worklist.len()));

        for (remove, _) in worklist {
            f.body.basic_blocks.order.remove_item(&remove).unwrap();
        }
    }
}
