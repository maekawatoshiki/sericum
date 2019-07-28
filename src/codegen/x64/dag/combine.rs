use super::{function::*, module::*, node::*};
use id_arena::*;
use rustc_hash::FxHashMap;

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
                self.combine_node(&mut FxHashMap::default(), &mut func.dag_arena, entry);
            })
        }
    }

    fn combine_node(
        &mut self,
        replace: &mut FxHashMap<DAGNodeId, DAGNodeId>,
        arena: &mut Arena<DAGNode>,
        node_id: DAGNodeId,
    ) -> DAGNodeId {
        if let Some(replaced) = replace.get(&node_id) {
            return *replaced;
        }

        // TODO: Macro for pattern matching?
        let replaced_id = match arena[node_id].kind {
            DAGNodeKind::BrCond => {
                let cond_id = arena[node_id].operand[0].id();
                let br = arena[node_id].operand[1].basic_block();
                match arena[cond_id].kind {
                    DAGNodeKind::Setcc => {
                        let c = arena[cond_id].operand[0].cond_kind();
                        let op0 = self.combine_node(replace, arena, arena[cond_id].operand[1].id());
                        let op1 = self.combine_node(replace, arena, arena[cond_id].operand[2].id());
                        arena.alloc(DAGNode::new(
                            DAGNodeKind::Brcc,
                            vec![
                                DAGNodeValue::CondKind(c),
                                DAGNodeValue::Id(op0),
                                DAGNodeValue::Id(op1),
                                DAGNodeValue::BasicBlock(br),
                            ],
                            None,
                        ))
                    }
                    _ => node_id,
                }
            }
            _ => node_id,
        };
        replace.insert(node_id, replaced_id);

        arena[replaced_id].next = match arena[node_id].next {
            Some(next) => Some(self.combine_node(replace, arena, next)),
            None => return replaced_id,
        };

        replaced_id
    }
}
