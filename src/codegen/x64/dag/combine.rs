use super::{function::*, module::*, node::*};
use id_arena::*;

pub struct Combine {}

impl Combine {
    pub fn new() -> Self {
        Self {}
    }

    pub fn combine_module(&mut self, module: &mut DAGModule) {
        for (_, func) in &mut module.functions {
            self.combine_function(func)
        }
    }

    fn combine_function(&mut self, func: &mut DAGFunction) {
        for (_, bb) in &func.dag_basic_blocks {
            some_then!(entry, bb.entry, {
                self.combine_node(&mut func.dag_arena, entry);
            })
        }
    }

    fn combine_node(&mut self, arena: &mut Arena<DAGNode>, node_id: DAGNodeId) -> DAGNodeId {
        // TODO: Macro for pattern matching?
        let replaced_id = match arena[node_id].kind {
            DAGNodeKind::BrCond(cond_id, br) => match arena[cond_id].kind {
                DAGNodeKind::Setcc(c, op0, op1) => {
                    arena.alloc(DAGNode::new(DAGNodeKind::Brcc(c, op0, op1, br), None))
                }
                _ => node_id,
            },
            _ => node_id,
        };

        arena[replaced_id].next = match arena[node_id].next {
            Some(next) => Some(self.combine_node(arena, next)),
            None => return replaced_id,
        };

        replaced_id
    }
}
