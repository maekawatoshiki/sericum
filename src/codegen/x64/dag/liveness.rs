use super::{basic_block::*, convert::*, node::*};
// use super::{convert::*, node::*};
use crate::ir::module::*;
// use id_arena::*;
// use rustc_hash::FxHashMap;

pub struct LivenessAnalysis<'a> {
    pub module: &'a Module,
    // pub dag_id_to_vreg: FxHashMap<DAGNodeId, usize>,
    // pub vreg_count: usize,
}

impl<'a> LivenessAnalysis<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    pub fn analyze_function(&mut self, dag_func: &DAGFunction) {
        self.set_def(dag_func);
        self.visit(dag_func);
    }

    fn set_def(&mut self, cur_func: &DAGFunction) {
        for (_, bb) in &cur_func.dag_basic_blocks {
            self.set_def_node(cur_func, bb, bb.entry.unwrap());
        }
    }

    fn set_def_node(&mut self, cur_func: &DAGFunction, bb: &DAGBasicBlock, node_id: DAGNodeId) {
        let node = &cur_func.dag_arena[node_id];
        match &node.kind {
            DAGNodeKind::Add(_, _) | DAGNodeKind::Setcc(_, _, _) | DAGNodeKind::Load(_) => {
                bb.liveness.borrow_mut().def.insert(node_id);
            }
            DAGNodeKind::Store(_, _)
            | DAGNodeKind::Entry
            | DAGNodeKind::Ret(_)
            | DAGNodeKind::FrameIndex(_, _)
            | DAGNodeKind::Br(_)
            | DAGNodeKind::BrCond(_, _)
            | DAGNodeKind::Constant(_) => {}
        }
    }

    fn visit(&mut self, cur_func: &DAGFunction) {
        for (bb_id, bb) in &cur_func.dag_basic_blocks {
            self.visit_node(cur_func, bb_id, bb.entry.unwrap());
        }
    }

    fn visit_node(&mut self, cur_func: &DAGFunction, bb_id: DAGBasicBlockId, node_id: DAGNodeId) {
        let node = &cur_func.dag_arena[node_id];

        match node.kind {
            DAGNodeKind::Add(op1, op2)
            | DAGNodeKind::Setcc(_, op1, op2)
            | DAGNodeKind::Store(op1, op2) => {
                self.propagate(cur_func, bb_id, op1);
                self.propagate(cur_func, bb_id, op2);
            }
            DAGNodeKind::Load(op1) | DAGNodeKind::BrCond(op1, _) | DAGNodeKind::Ret(op1) => {
                self.propagate(cur_func, bb_id, op1);
            }
            DAGNodeKind::Entry
            | DAGNodeKind::FrameIndex(_, _)
            | DAGNodeKind::Br(_)
            | DAGNodeKind::Constant(_) => {}
        }

        some_then!(next, node.next, self.visit_node(cur_func, bb_id, next));
    }

    fn propagate(&self, cur_func: &DAGFunction, bb_id: DAGBasicBlockId, node_id: DAGNodeId) {
        let bb = &cur_func.dag_basic_blocks[bb_id];

        {
            let mut bb_liveness = bb.liveness.borrow_mut();

            if bb_liveness.def.contains(&node_id) {
                return;
            }

            if !bb_liveness.live_in.insert(node_id) {
                // live_in already had the value instr_id
                return;
            }
        }

        for pred_id in &bb.pred {
            let pred = &cur_func.dag_basic_blocks[*pred_id];
            if pred.liveness.borrow_mut().live_out.insert(node_id) {
                // live_out didn't have the value instr_id
                self.propagate(cur_func, *pred_id, node_id);
            }
        }
    }
}
