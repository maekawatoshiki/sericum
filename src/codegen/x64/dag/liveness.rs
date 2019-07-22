use super::{basic_block::*, convert::*, node::*};
// use super::{convert::*, node::*};
use crate::ir::module::*;
// use id_arena::*;
use rustc_hash::FxHashSet;

pub struct LivenessAnalysis<'a> {
    pub module: &'a Module,
    vreg_count: usize,
    // pub dag_id_to_vreg: FxHashMap<DAGNodeId, usize>,
    // pub vreg_count: usize,
}

impl<'a> LivenessAnalysis<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            vreg_count: 0,
        }
    }

    pub fn analyze_function(&mut self, dag_func: &DAGFunction) {
        self.set_def(dag_func);
        self.visit(dag_func);
    }

    fn next_vreg(&mut self) -> usize {
        self.vreg_count += 1;
        self.vreg_count
    }

    fn set_def(&mut self, cur_func: &DAGFunction) {
        for (_, bb) in &cur_func.dag_basic_blocks {
            let mut marked = FxHashSet::default();
            self.set_def_node(&mut marked, cur_func, bb, bb.entry.unwrap());
        }
    }

    fn set_def_node(
        &mut self,
        marked: &mut FxHashSet<DAGNodeId>,
        cur_func: &DAGFunction,
        bb: &DAGBasicBlock,
        node_id: DAGNodeId,
    ) {
        if !marked.insert(node_id) {
            return;
        }

        let node = &cur_func.dag_arena[node_id];
        node.set_vreg(self.next_vreg());

        match &node.kind {
            DAGNodeKind::Store(op1, op2) => {
                self.set_def_node(marked, cur_func, bb, *op1);
                self.set_def_node(marked, cur_func, bb, *op2);
            }
            DAGNodeKind::Add(op1, op2) | DAGNodeKind::Setcc(_, op1, op2) => {
                self.set_def_node(marked, cur_func, bb, *op1);
                self.set_def_node(marked, cur_func, bb, *op2);
                bb.liveness.borrow_mut().def.insert(node_id);
            }
            DAGNodeKind::Ret(op1) | DAGNodeKind::BrCond(op1, _) => {
                self.set_def_node(marked, cur_func, bb, *op1);
            }
            DAGNodeKind::Load(op1) => {
                self.set_def_node(marked, cur_func, bb, *op1);
                bb.liveness.borrow_mut().def.insert(node_id);
            }
            DAGNodeKind::Entry
            | DAGNodeKind::FrameIndex(_, _)
            | DAGNodeKind::Br(_)
            | DAGNodeKind::Constant(_) => {}
        }

        some_then!(
            next,
            node.next,
            self.set_def_node(marked, cur_func, bb, next)
        );
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
        match cur_func.dag_arena[node_id].kind {
            DAGNodeKind::Entry
            | DAGNodeKind::FrameIndex(_, _)
            | DAGNodeKind::Br(_)
            | DAGNodeKind::Constant(_) => return,
            _ => {}
        }

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
