use crate::{
    analysis::{
        dom_tree::{DominatorTree, DominatorTreeConstructor},
        loops::{Loop, Loops, LoopsConstructor},
    },
    ir::{
        basic_block::{BasicBlock, BasicBlockId},
        builder::{Builder, FuncRef, FunctionEntity},
        function::Function,
        module::Module,
        opcode::{Instruction, Opcode, Operand},
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

struct SimplifyLoopOnFunction<'a> {
    func: &'a mut Function,
}

struct BackEdges {
    edges: Vec<BasicBlockId>,
    dest: BasicBlockId,
}

impl LoopInvariantCodeMotion {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
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
        SimplifyLoopOnFunction::new(self.func).run();

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

        let mut preds = self.func.basic_blocks.arena[loop_.header].pred.clone();
        preds.retain(|p| !loop_.contains(p));
        let preds_not_in_loop = preds;

        assert!(preds_not_in_loop.len() == 1);

        let header_bb = &mut self.func.basic_blocks.arena[loop_.header];
        let header_preds = &mut header_bb.pred;
        header_preds.retain(|p| !preds_not_in_loop.contains(p)); // retain preds in loop
        header_preds.insert(pre_header);
        for &id in header_bb.iseq_ref().iter().rev() {
            let id = id.as_instruction().id;
            if self.func.inst_table[id].opcode == Opcode::Phi {
                Instruction::replace_operand(
                    &mut self.func.inst_table,
                    id,
                    &Operand::BasicBlock(*preds_not_in_loop.iter().next().unwrap()),
                    Operand::BasicBlock(pre_header),
                );
            }
        }

        for &pred in &preds_not_in_loop {
            let block = &mut self.func.basic_blocks.arena[pred];
            block.succ.retain(|&s| s != loop_.header);
            block.succ.insert(pre_header);
            for &id in block.iseq_ref().iter().rev() {
                let id = id.as_instruction().id;
                if !self.func.inst_table[id].opcode.is_terminator() {
                    break;
                }
                Instruction::replace_operand(
                    &mut self.func.inst_table,
                    id,
                    &Operand::BasicBlock(loop_.header),
                    Operand::BasicBlock(pre_header),
                );
            }
        }

        self.func.basic_blocks.arena[pre_header]
            .pred
            .extend(preds_not_in_loop);

        let mut builder = Builder::new(FunctionEntity(self.func));
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
                for inst_id in bb.iseq.borrow().iter().map(|v| v.as_instruction().id) {
                    worklist.push_back(inst_id);
                }
            }

            while worklist.len() > 0 {
                let mut insts_to_hoist = vec![];
                while let Some(inst_id) = worklist.pop_front() {
                    let inst = &self.func.inst_table[inst_id];
                    if inst.opcode.access_memory()
                        || inst.opcode == Opcode::Call
                        // TODO: Remove the line below after implementing CodeGenPrepare
                        || inst.opcode == Opcode::GetElementPtr
                    {
                        continue;
                    }
                    let invariant = inst.operands.iter().all(|operand| match operand {
                        Operand::Value(Value::Instruction(InstructionValue { id, .. })) => {
                            let inst = &self.func.inst_table[*id];
                            !loop_.contains(&inst.parent)
                        }
                        Operand::Value(_) => true,
                        _ => false,
                    });
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

                    let mut builder = Builder::new(FunctionEntity(self.func));
                    builder.set_insert_point_before_terminator(pre_header);
                    builder.insert(val);
                }
            }
        }

        debug!(println!("LICM: {} invariants hoisted", count));
    }
}

impl<'a> SimplifyLoopOnFunction<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self { func }
    }

    pub fn run(&mut self) {
        let dom_tree = DominatorTreeConstructor::new(&self.func.basic_blocks).construct();
        let backedges_to_merge = self.collect_backedges_to_merge(&dom_tree);
        for back_edges in backedges_to_merge {
            self.merge_backedges(back_edges);
        }
    }

    fn collect_backedges_to_merge(
        &mut self,
        dom_tree: &DominatorTree<BasicBlock>,
    ) -> Vec<BackEdges> {
        let post_order = dom_tree.post_ordered_blocks(None);
        let mut backedges_to_merge = vec![];

        for node in &post_order {
            let mut back_edges = vec![];
            let header = *node;

            let preds = &self.func.basic_blocks.arena[header].pred;
            for back_edge in preds {
                if dom_tree.dominate_bb(header, *back_edge)
                /* && back_edge is reachable from entry*/
                {
                    back_edges.push(*back_edge);
                }
            }

            if back_edges.len() > 1 {
                // multiple backedges
                backedges_to_merge.push(BackEdges {
                    edges: back_edges,
                    dest: header,
                });
            }
        }

        backedges_to_merge
    }

    fn merge_backedges(&mut self, back_edges: BackEdges) {
        let merge = self.func.append_basic_block_before(back_edges.edges[0]);

        for &edge in &back_edges.edges {
            let edge_ = &mut self.func.basic_blocks.arena[edge];
            edge_.succ.remove(&back_edges.dest);
            edge_.succ.insert(merge);

            for id in edge_
                .iseq_ref()
                .iter()
                .rev()
                .map(|id| id.as_instruction().id)
            {
                if self.func.inst_table[id].opcode.is_terminator() {
                    Instruction::replace_operand(
                        &mut self.func.inst_table,
                        id,
                        &Operand::BasicBlock(back_edges.dest),
                        Operand::BasicBlock(merge),
                    );
                }
            }

            let merge_ = &mut self.func.basic_blocks.arena[merge];
            merge_.pred.insert(edge);
            let dest_ = &mut self.func.basic_blocks.arena[back_edges.dest];
            dest_.pred.remove(&edge);
        }

        let mut builder = Builder::new(FunctionEntity(self.func));
        builder.set_insert_point(merge);
        builder.build_br(back_edges.dest);

        self.move_phi(&back_edges, merge)
    }

    fn move_phi(&mut self, back_edges: &BackEdges, merge: BasicBlockId) {
        let dest_ = &mut self.func.basic_blocks.arena[back_edges.dest];
        let mut new_phi_incomings = vec![];

        for id in dest_.iseq_ref().iter().map(|id| id.as_instruction().id) {
            if self.func.inst_table[id].opcode != Opcode::Phi {
                continue;
            }
            let inst = &mut self.func.inst_table[id];
            let mut phi_incomings = vec![];
            for i in (0..inst.operands.len()).step_by(2) {
                let val = inst.operands[i];
                let block = inst.operands[i + 1];
                if back_edges.edges.contains(block.as_basic_block()) {
                    phi_incomings.push((*val.as_value(), *block.as_basic_block()));
                }
            }
            for (_val, block) in &phi_incomings {
                let i = inst
                    .operands
                    .iter()
                    .position(|op| {
                        matches!(op, Operand::BasicBlock(_)) && op.as_basic_block() == block
                    })
                    .unwrap();
                inst.operands.remove(i); // remove block
                inst.operands.remove(i - 1); // remove val
            }
            if phi_incomings.len() > 0 {
                new_phi_incomings.push((id, phi_incomings));
            }
        }

        let mut builder = Builder::new(FunctionEntity(self.func));
        builder.set_insert_point_at(0, merge);
        for (phi, incomings) in new_phi_incomings {
            let new_phi = builder.build_phi(incomings);
            let phi = &mut builder.func.func_ref_mut().inst_table[phi];
            phi.operands.push(Operand::Value(new_phi));
            phi.operands.push(Operand::BasicBlock(merge));
        }
    }
}
