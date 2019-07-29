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

        let new_operands = self.combine_operands(replace, arena, arena[node_id].operand.clone());
        arena[node_id].operand = new_operands;

        // TODO: Macro for pattern matching?
        let replaced_id = match &arena[node_id].kind {
            DAGNodeKind::Add => self.combine_node_add(arena, node_id),
            DAGNodeKind::BrCond => self.combine_node_brcond(replace, arena, node_id),
            _ => node_id,
        };
        replace.insert(node_id, replaced_id);

        arena[replaced_id].next = match arena[node_id].next {
            Some(next) => Some(self.combine_node(replace, arena, next)),
            None => return replaced_id,
        };

        replaced_id
    }

    fn combine_node_add(&mut self, arena: &mut Arena<DAGNode>, node_id: DAGNodeId) -> DAGNodeId {
        #[rustfmt::skip]
        macro_rules! node { () => { arena[node_id] }; }

        // (C + n) -> (n + C)
        if node!().operand[0].is_constant() && !node!().operand[1].is_constant() {
            node!().operand.swap(0, 1);
        }

        // ((a + C1) + C2) -> (a + (C1 + C2))
        if node!().operand[0].is_id()
            && arena[node!().operand[0].id()].kind == DAGNodeKind::Add
            && !arena[node!().operand[0].id()].operand[0].is_constant()
            && arena[node!().operand[0].id()].operand[1].is_constant()
            && node!().operand[1].is_constant()
        {
            let op0 = arena[node!().operand[0].id()].operand[0].clone();
            let c = DAGNodeValue::Constant(
                arena[node!().operand[0].id()].operand[1]
                    .constant()
                    .add(node!().operand[1].constant()),
            );
            return arena.alloc(DAGNode::new(
                DAGNodeKind::Add,
                vec![op0, c],
                node!().ty.clone(),
            ));
        }

        node_id
    }

    fn combine_node_val(
        &mut self,
        replace: &mut FxHashMap<DAGNodeId, DAGNodeId>,
        arena: &mut Arena<DAGNode>,
        val: DAGNodeValue,
    ) -> DAGNodeValue {
        match val {
            DAGNodeValue::Id(id) => DAGNodeValue::Id(self.combine_node(replace, arena, id)),
            _ => val,
        }
    }

    fn combine_node_brcond(
        &mut self,
        replace: &mut FxHashMap<DAGNodeId, DAGNodeId>,
        arena: &mut Arena<DAGNode>,
        node_id: DAGNodeId,
    ) -> DAGNodeId {
        let node = &arena[node_id];
        let cond_id = node.operand[0].id();
        let br = node.operand[1].clone();
        match arena[cond_id].kind {
            DAGNodeKind::Setcc => {
                let op0 = self.combine_node_val(replace, arena, arena[cond_id].operand[1].clone());
                let op1 = self.combine_node_val(replace, arena, arena[cond_id].operand[2].clone());
                arena.alloc(DAGNode::new(
                    DAGNodeKind::Brcc,
                    vec![arena[cond_id].operand[0].clone(), op0, op1, br],
                    None,
                ))
            }
            _ => node_id,
        }
    }

    fn combine_operands(
        &mut self,
        replace: &mut FxHashMap<DAGNodeId, DAGNodeId>,
        arena: &mut Arena<DAGNode>,
        operands: Vec<DAGNodeValue>,
    ) -> Vec<DAGNodeValue> {
        operands
            .into_iter()
            .map(|op| self.combine_node_val(replace, arena, op))
            .collect()
    }
}
