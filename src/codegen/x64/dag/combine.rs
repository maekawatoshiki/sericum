use super::{function::*, module::*, node::*};
use crate::ir::types::*;
use crate::util::allocator::*;
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
        for bb_id in &func.dag_basic_blocks {
            let bb = &func.dag_basic_block_arena[*bb_id];
            some_then!(entry, bb.entry, {
                self.combine_node(&mut FxHashMap::default(), &mut func.dag_heap, entry);
            })
        }
    }

    fn combine_node(
        &mut self,
        replace: &mut FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
        heap: &mut RawAllocator<DAGNode>,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if !node.is_operation() {
            return node;
        }

        if let Some(replaced) = replace.get(&node) {
            return *replaced;
        }

        let new_operands = self.combine_operands(replace, heap, node.operand.clone());
        node.operand = new_operands;

        // TODO: Macro for pattern matching?
        let mut replaced = match &node.kind {
            NodeKind::IR(IRNodeKind::Load) => self.combine_node_load(heap, node),
            NodeKind::IR(IRNodeKind::Store) => self.combine_node_store(heap, node),
            NodeKind::IR(IRNodeKind::Add) => self.combine_node_add(replace, heap, node),
            NodeKind::IR(IRNodeKind::BrCond) => self.combine_node_brcond(heap, node),
            _ => node,
        };
        replace.insert(node, replaced);

        replaced.next = match node.next {
            Some(next) => Some(self.combine_node(replace, heap, next)),
            None => return replaced,
        };

        replaced
    }

    fn combine_node_add(
        &mut self,
        replace: &mut FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
        heap: &mut RawAllocator<DAGNode>,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        // (C + any) -> (any + C)
        if node.operand[0].is_constant() && !node.operand[1].is_constant() {
            node.operand.swap(0, 1);
        }

        // (~fi + fi) -> (fi + ~fi)
        if !node.operand[0].is_frame_index() && node.operand[1].is_frame_index() {
            node.operand.swap(0, 1);
        }

        // println!(">>>> {:?}", node_op!(1));
        if node.operand[1].is_constant() && node.operand[1].as_constant().is_null() {
            return node.operand[0];
        }

        // ((node + C1) + C2) -> (node + (C1 + C2))
        if node.operand[0].is_operation()
            && node.operand[0].kind == NodeKind::IR(IRNodeKind::Add)
            && !node.operand[0].operand[0].is_constant()
            && node.operand[0].operand[1].is_constant()
            && node.operand[1].is_constant()
        {
            let op0 = self.combine_node(replace, heap, node.operand[0].operand[0]);
            let const_folded = node.operand[0].operand[1]
                .as_constant()
                .add(node.operand[1].as_constant());
            let c = heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::Constant(const_folded)),
                vec![],
                const_folded.get_type(),
            ));
            return heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::Add),
                vec![op0, c],
                node.ty.clone(),
            ));
        }

        node
    }

    fn combine_node_load(
        &mut self,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].is_operation() && node.operand[0].kind == NodeKind::IR(IRNodeKind::Add) {
            let add = node.operand[0].clone();
            let op0 = add.operand[0];
            let op1 = add.operand[1];

            if op0.is_frame_index() && op1.is_constant() {
                return heap.alloc(DAGNode::new(
                    NodeKind::IR(IRNodeKind::LoadFiConstOff),
                    vec![op0, op1],
                    node.ty.clone(),
                ));
            }

            if op0.is_frame_index()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                return heap.alloc(DAGNode::new(
                    NodeKind::IR(IRNodeKind::LoadFiOff),
                    vec![op0, op1.operand[0], op1.operand[1]],
                    node.ty.clone(),
                ));
            }

            if op0.is_operation()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                return heap.alloc(DAGNode::new(
                    NodeKind::IR(IRNodeKind::LoadRegOff),
                    vec![op0, op1.operand[0], op1.operand[1]],
                    node.ty.clone(),
                ));
            }
        }

        node
    }

    fn combine_node_store(
        &mut self,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].kind == NodeKind::IR(IRNodeKind::Add) {
            let add = node.operand[0].clone();
            let op0 = add.operand[0];
            let op1 = add.operand[1];
            let new_src = node.operand[1];

            if op0.is_frame_index() && op1.is_constant() {
                return heap.alloc(DAGNode::new(
                    NodeKind::IR(IRNodeKind::StoreFiConstOff),
                    vec![op0, op1, new_src],
                    node.ty.clone(),
                ));
            }

            if op0.is_frame_index()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                return heap.alloc(DAGNode::new(
                    NodeKind::IR(IRNodeKind::StoreFiOff),
                    vec![op0, op1.operand[0], op1.operand[1], new_src],
                    node.ty.clone(),
                ));
            }

            if op0.is_operation()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                return heap.alloc(DAGNode::new(
                    NodeKind::IR(IRNodeKind::StoreRegOff),
                    vec![op0, op1.operand[0], op1.operand[1], new_src],
                    node.ty.clone(),
                ));
            }
        }

        node
    }

    fn combine_node_brcond(
        &mut self,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let cond = node.operand[0];
        let br = node.operand[1];
        match cond.kind {
            NodeKind::IR(IRNodeKind::Setcc) => heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::Brcc),
                vec![cond.operand[0], cond.operand[1], cond.operand[2], br],
                Type::Void,
            )),
            _ => node,
        }
    }

    fn combine_operands(
        &mut self,
        replace: &mut FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
        heap: &mut RawAllocator<DAGNode>,
        operands: Vec<Raw<DAGNode>>,
    ) -> Vec<Raw<DAGNode>> {
        operands
            .into_iter()
            .map(|op| self.combine_node(replace, heap, op))
            .collect()
    }
}
