use super::{basic_block::*, convert::*, node::*};
// use super::{convert::*, node::*};
use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
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
        for (_, bb) in &dag_func.dag_basic_blocks {
            self.analyze_node(dag_func, bb.entry.unwrap());
        }
    }

    fn set_def(&mut self, dag_func: &DAGFunction) {
        for (_, bb) in &dag_func.dag_basic_blocks {
            self.set_def_node(dag_func, bb, bb.entry.unwrap());
        }
    }

    fn set_def_node(&mut self, dag_func: &DAGFunction, bb: &DAGBasicBlock, node_id: DAGNodeId) {
        let node = &dag_func.dag_arena[node_id];
        match &node.kind {
            DAGNodeKind::Add(_, _) | DAGNodeKind::Setcc(_, _, _) | DAGNodeKind::Load(_) => {
                bb.liveness.borrow_mut().def.insert(node_id);
            }
            _ => {}
        }
    }

    fn analyze_node(&mut self, cur_dag_func: &DAGFunction, node_id: DAGNodeId) {
        let node = &cur_dag_func.dag_arena[node_id];
    }
}
