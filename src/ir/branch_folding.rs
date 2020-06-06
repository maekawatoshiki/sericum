use crate::ir::{
    function::Function,
    module::Module,
    opcode::{Instruction, Opcode, Operand},
};
use rustc_hash::FxHashSet;

pub struct BranchFolding {}

pub struct BranchFoldingOnFunction<'a> {
    func: &'a mut Function,
}

impl BranchFolding {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            BranchFoldingOnFunction::new(func).run()
        }
    }
}

impl<'a> BranchFoldingOnFunction<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self { func }
    }

    // TODO: Refine code
    pub fn run(&mut self) {
        let mut phi_incoming_blocks = FxHashSet::default();
        let mut worklist = vec![];

        for &bb_id in &self.func.basic_blocks.order {
            let bb = &self.func.basic_blocks.arena[bb_id];
            for val in &*bb.iseq.borrow() {
                let inst = &self.func.inst_table[val.get_inst_id().unwrap()];
                if inst.opcode == Opcode::Phi {
                    for operand in &inst.operands {
                        if let Operand::BasicBlock(bb) = operand {
                            phi_incoming_blocks.insert(*bb);
                        }
                    }
                }
            }
        }

        for &bb_id in &self.func.basic_blocks.order {
            if phi_incoming_blocks.contains(&bb_id) {
                continue;
            }
            let bb = &self.func.basic_blocks.arena[bb_id];
            let iseq = bb.iseq.borrow();
            if iseq.len() == 0 || iseq.len() > 1 {
                continue;
            }
            let inst = &self.func.inst_table[iseq[0].get_inst_id().unwrap()];
            if inst.opcode == Opcode::Br {
                worklist.push((bb_id, *inst.operands[0].as_basic_block()));
            }
        }

        for &(block_to_remove, new_dst) in &worklist {
            let preds = self.func.basic_blocks.arena[block_to_remove].pred.clone();
            let succs = self.func.basic_blocks.arena[block_to_remove].succ.clone();

            for &bb in &preds {
                let cur = &mut self.func.basic_blocks.arena[bb];
                cur.succ.remove_item(&block_to_remove).unwrap();
                cur.succ.push(new_dst);
                if cur.pred.remove_item(&block_to_remove).is_some() {
                    cur.pred.append(&mut preds.clone());
                }
            }

            for &bb in &succs {
                let cur = &mut self.func.basic_blocks.arena[bb];
                cur.pred.remove_item(&block_to_remove).unwrap();
                cur.pred.append(&mut preds.clone());
                if cur.succ.remove_item(&block_to_remove).is_some() {
                    cur.succ.push(new_dst);
                }
            }

            for &bb in &preds {
                let cur = &mut self.func.basic_blocks.arena[bb];
                for val in cur.iseq.borrow().iter().rev() {
                    let inst_id = val.get_inst_id().unwrap();
                    Instruction::replace_operand(
                        &mut self.func.inst_table,
                        inst_id,
                        &Operand::BasicBlock(block_to_remove),
                        Operand::BasicBlock(new_dst),
                    );
                }
            }

            for &bb in &succs {
                let cur = &mut self.func.basic_blocks.arena[bb];
                for val in cur.iseq.borrow().iter().rev() {
                    let inst_id = val.get_inst_id().unwrap();
                    assert!(preds.len() <= 1);
                    if preds.len() == 0 {
                        continue;
                    }
                    Instruction::replace_operand(
                        &mut self.func.inst_table,
                        inst_id,
                        &Operand::BasicBlock(block_to_remove),
                        Operand::BasicBlock(preds[0]),
                    );
                }
            }
        }

        debug!(println!("{} blocks removed", worklist.len()));

        for (remove, _) in worklist {
            self.func.basic_blocks.order.remove_item(&remove).unwrap();
        }
    }
}
