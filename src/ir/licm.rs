use crate::{
    analysis::{
        dom_tree::DominatorTreeConstructor,
        loops::{Loop, Loops, LoopsConstructor},
    },
    ir::{
        basic_block::{BasicBlock, BasicBlockId},
        builder::IRBuilder,
        function::Function,
        module::Module,
        opcode::{Instruction, Opcode},
        simplify_loop::SimplifyLoop,
        value::*,
    },
};
use id_arena::Id;
use rustc_hash::FxHashMap;
use std::collections::VecDeque;

pub struct LoopInvariantCodeMotion {}

struct LoopInvariantCodeMotionOnFunction<'a> {
    func: &'a mut Function,
}

impl LoopInvariantCodeMotion {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        SimplifyLoop::new().run_on_module(module);

        for (_, func) in &mut module.functions {
            if func.is_internal || func.is_empty() {
                continue;
            }
            LoopInvariantCodeMotionOnFunction::new(func).run();
        }
    }
}

impl<'a> LoopInvariantCodeMotionOnFunction<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self { func }
    }

    pub fn run(&mut self) {
        let dom_tree = DominatorTreeConstructor::new(&self.func.basic_blocks).construct();
        let mut loops = LoopsConstructor::new(&dom_tree, &self.func.basic_blocks).analyze();

        let pre_headers = self.insert_pre_headers(&mut loops);

        self.hoist_invariants(&loops, &pre_headers);
    }

    fn insert_pre_headers(
        &mut self,
        loops: &mut Loops<BasicBlock>,
    ) -> FxHashMap<Id<Loop<BasicBlock>>, BasicBlockId> /* loop id -> pre header */ {
        let mut pre_headers = FxHashMap::default();

        for (id, loop_) in &loops.arena {
            let pre_header = self.insert_pre_header(&loop_);
            pre_headers.insert(id, pre_header);
        }

        for (id, &pre_header) in &pre_headers {
            let mut id = *id;
            while let Some(parent) = loops.arena[id].parent {
                loops.arena[parent].set.insert(pre_header);
                id = parent;
            }
        }

        pre_headers
    }

    fn insert_pre_header(&mut self, loop_: &Loop<BasicBlock>) -> BasicBlockId {
        let pre_header = self.func.append_basic_block_before(loop_.header);

        // self.func.basic_blocks.delete_edge(from, loop_.header);

        let mut preds = self.func.basic_blocks.arena[loop_.header].pred.clone();
        preds.retain(|p| !loop_.contains(p));
        let preds_outside_loop = preds;

        assert!(preds_outside_loop.len() == 1);

        let header_bb = &mut self.func.basic_blocks.arena[loop_.header];
        let header_preds = &mut header_bb.pred;
        header_preds.retain(|p| !preds_outside_loop.contains(p)); // retain preds in loop
        header_preds.insert(pre_header);
        for &id in header_bb.iseq_ref().iter().rev() {
            if self.func.inst_table[id].opcode == Opcode::Phi {
                Instruction::replace_block_operand(
                    &mut self.func.inst_table,
                    id,
                    preds_outside_loop.iter().next().unwrap(),
                    pre_header,
                );
            }
        }

        for &pred in &preds_outside_loop {
            let block = &mut self.func.basic_blocks.arena[pred];
            block.succ.retain(|&s| s != loop_.header);
            block.succ.insert(pre_header);
            for &id in block.iseq_ref().iter().rev() {
                if !self.func.inst_table[id].opcode.is_terminator() {
                    break;
                }
                Instruction::replace_block_operand(
                    &mut self.func.inst_table,
                    id,
                    &loop_.header,
                    pre_header,
                );
            }
        }

        for pred in preds_outside_loop {
            self.func.basic_blocks.make_edge(pred, pre_header);
        }

        let mut builder = self.func.ir_builder();
        builder.set_insert_point(pre_header);
        builder.build_br(loop_.header);

        pre_header
    }

    fn hoist_invariants(
        &mut self,
        loops: &Loops<BasicBlock>,
        pre_headers: &FxHashMap<Id<Loop<BasicBlock>>, BasicBlockId>,
    ) {
        let mut count = 0;

        for (id, loop_) in &loops.arena {
            let mut worklist = VecDeque::new();

            for &bb_id in &loop_.set {
                let bb = &self.func.basic_blocks.arena[bb_id];
                for &inst_id in &*bb.iseq_ref() {
                    worklist.push_back(inst_id);
                }
            }

            while worklist.len() > 0 {
                let mut insts_to_hoist = vec![];
                while let Some(inst_id) = worklist.pop_front() {
                    let inst = &self.func.inst_table[inst_id];
                    if inst.opcode.access_memory() || inst.opcode == Opcode::Call {
                        continue;
                    }
                    let invariant = inst.operand.args().iter().all(|val| match val {
                        Value::Instruction(InstructionValue { id, .. }) => {
                            let inst = &self.func.inst_table[*id];
                            !loop_.contains(&inst.parent)
                        }
                        _ => true,
                    }) && inst.operand.blocks().len() == 0
                        && inst.operand.types().len() == 0;
                    if invariant {
                        count += 1;
                        insts_to_hoist.push(inst_id);
                    }
                }

                for inst_id in insts_to_hoist {
                    let pre_header = pre_headers[&id];
                    let val = self.func.remove_inst_from_block(inst_id);
                    let inst = &mut self.func.inst_table[inst_id];
                    inst.parent = pre_header;

                    for &user in &*inst.users.borrow() {
                        worklist.push_back(user);
                    }
                    // worklist.push_back(inst_id);

                    let mut builder = self.func.ir_builder();
                    builder.set_insert_point_before_terminator(pre_header);
                    builder.insert(val);
                }
            }
        }

        debug!(println!("LICM: {} invariants hoisted", count));
    }
}
