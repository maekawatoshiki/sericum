use super::{convert::*, node::*};
use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;
use rustc_hash::FxHashMap;

pub struct SelectInstruction<'a> {
    pub module: &'a Module,
    // pub dag_arena: Arena<DAGNode>,
    // pub instr_id_to_dag_node_id: FxHashMap<InstructionId, DAGNodeId>,
}

impl<'a> SelectInstruction<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    pub fn select_function(&mut self, dag_func: &DAGFunction) {
        for (_, node) in &dag_func.bb_to_node {
            self.select_dag(&dag_func, *node);
        }
    }

    pub fn select_dag(&mut self, dag_func: &DAGFunction, node_id: DAGNodeId) {
        let node = &dag_func.dag_arena[node_id];

        // let new_kind = match node.kind {
        //     DAGNodeKind::Entry => DAGNodeKind::Entry,
        //     DAGNodeKind::Load(nid) => {}
        // };
    }
}
