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

        let mut updated = false;
        // let mut new_operands = vec![];
        let mut new_operands = vec![];
        for op in arena[node_id].operand.clone() {
            new_operands.push(match op {
                DAGNodeValue::Id(id) => {
                    let new_id = self.combine_node(replace, arena, id);
                    updated = id != new_id;
                    DAGNodeValue::Id(new_id)
                }
                _ => op,
            });
        }
        // .iter()
        // .map(|op| match op {
        //     DAGNodeValue::Id(id) => {
        //         let new_id = self.combine_node(replace, arena, *id);
        //         updated = *id != new_id;
        //         DAGNodeValue::Id(new_id)
        //     }
        //     _ => op.clone(),
        // })
        // .collect::<Vec<DAGNodeValue>>();

        // TODO: Macro for pattern matching?
        let node = &arena[node_id];
        let replaced_id = match &node.kind {
            DAGNodeKind::BrCond => {
                let cond_id = node.operand[0].id();
                let br = node.operand[1].clone();
                match arena[cond_id].kind {
                    DAGNodeKind::Setcc => {
                        let op0 = self.combine_node(replace, arena, arena[cond_id].operand[1].id());
                        let op1 = self.combine_node(replace, arena, arena[cond_id].operand[2].id());
                        arena.alloc(DAGNode::new(
                            DAGNodeKind::Brcc,
                            vec![
                                arena[cond_id].operand[0].clone(),
                                DAGNodeValue::Id(op0),
                                DAGNodeValue::Id(op1),
                                br,
                            ],
                            None,
                        ))
                    }
                    _ => node_id,
                }
            }
            _ if updated => {
                let a = DAGNode::new(node.kind, new_operands, node.ty.clone());
                arena.alloc(a)
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
