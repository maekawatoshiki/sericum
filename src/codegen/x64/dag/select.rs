use super::{convert::*, node::*};
use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;
use rustc_hash::FxHashMap;

pub struct SelectInstruction<'a> {
    pub module: &'a Module,
}

impl<'a> SelectInstruction<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    pub fn select_function(&mut self, dag_func: &DAGFunction) {
        let mut dag_arena = Arena::new();
        for (_, node) in &dag_func.bb_to_node {
            self.select_dag(&dag_func, &mut dag_arena, *node);
        }
    }

    pub fn select_dag(
        &mut self,
        dag_func: &DAGFunction,
        dag_arena: &mut Arena<DAGNode>,
        node_id: DAGNodeId,
    ) -> DAGNodeId {
        let node = &dag_func.dag_arena[node_id];

        match node.kind {
            DAGNodeKind::Entry => {
                let node = DAGNode::new(DAGNodeKind::Entry, None).set_next(self.select_dag(
                    dag_func,
                    dag_arena,
                    node.next.unwrap(),
                ));
                dag_arena.alloc(node)
            }
            DAGNodeKind::Load(v_id) => {
                let v = &dag_arena[v_id];
                let node = match v.kind {
                    DAGNodeKind::FrameIndex(i) => {
                        DAGNode::new(DAGNodeKind::Load(v_id), node.ty.clone())
                            .set_operation("mov")
                            .set_next(self.select_dag(dag_func, dag_arena, node.next.unwrap()))
                    }
                    _ => unimplemented!(),
                };
                dag_arena.alloc(node)
            }
            _ => unimplemented!(),
        }
    }
}
