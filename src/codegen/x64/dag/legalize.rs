use super::{function::DAGFunction, module::DAGModule, node::*};
use crate::ir::types::*;
use crate::util::allocator::*;
use rustc_hash::FxHashMap;

pub struct Legalize {
    selected: FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
}

impl Legalize {
    pub fn new() -> Self {
        Self {
            selected: FxHashMap::default(),
        }
    }

    pub fn run_on_module(&mut self, module: &mut DAGModule) {
        for (_, func) in &mut module.functions {
            self.run_on_function(func)
        }
    }

    fn run_on_function(&mut self, func: &mut DAGFunction) {
        for bb_id in &func.dag_basic_blocks {
            let bb = &func.dag_basic_block_arena[*bb_id];
            self.run_on_node(&mut func.dag_heap, bb.entry.unwrap());
        }
    }

    fn run_on_node(
        &mut self,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if !node.is_operation() {
            return node;
        }

        if let Some(node) = self.selected.get(&node) {
            return *node;
        }

        // TODO: auto-generate the following code by macro
        let mut selected = match node.kind {
            NodeKind::IR(IRNodeKind::Load) => self.run_on_node_load(heap, node),
            NodeKind::IR(IRNodeKind::Store) => self.run_on_node_store(heap, node),
            NodeKind::IR(IRNodeKind::Add) => self.run_on_node_add(heap, node),
            _ => {
                self.run_on_node_operand(heap, node);
                node
            }
        };

        self.selected.insert(node, selected);

        if let Some(next) = node.next {
            selected.next = Some(self.run_on_node(heap, next));
        }

        selected
    }

    fn run_on_node_load(
        &mut self,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].kind == NodeKind::IR(IRNodeKind::Add) {
            let add = node.operand[0];
            let op0 = self.run_on_node(heap, add.operand[0]);
            let op1 = self.run_on_node(heap, add.operand[1]);

            if op0.is_frame_index() && op1.is_constant() {
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrmi32),
                    vec![op0, op1],
                    node.ty.clone(),
                ));
            }

            if op0.is_frame_index()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                let op1_op0 = self.run_on_node(heap, op1.operand[0]);
                let op1_op1 = op1.operand[1];
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrmri32),
                    vec![op0, op1_op0, op1_op1],
                    node.ty.clone(),
                ));
            }

            if op0.is_maybe_register()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                let op1_op0 = self.run_on_node(heap, op1.operand[0]);
                let op1_op1 = op1.operand[1];
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrrri32),
                    vec![op0, op1_op0, op1_op1],
                    node.ty.clone(),
                ));
            }
        }

        self.run_on_node_operand(heap, node);
        node
    }

    fn run_on_node_store(
        &mut self,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].kind == NodeKind::IR(IRNodeKind::Add) {
            let add = node.operand[0];
            let op0 = self.run_on_node(heap, add.operand[0]);
            let op1 = self.run_on_node(heap, add.operand[1]);
            let new_src = self.run_on_node(heap, node.operand[1]);

            if op0.is_frame_index() && op1.is_constant() {
                return heap.alloc(DAGNode::new(
                    if new_src.is_maybe_register() && new_src.ty == Type::Int32 {
                        NodeKind::MI(MINodeKind::MOVmi32r32)
                    } else if new_src.is_constant() && new_src.ty == Type::Int32 {
                        NodeKind::MI(MINodeKind::MOVmi32i32)
                    } else {
                        unimplemented!()
                    },
                    vec![op0, op1, new_src],
                    node.ty.clone(),
                ));
            }

            if op0.is_frame_index()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                let op1_op0 = self.run_on_node(heap, op1.operand[0]);
                let op1_op1 = op1.operand[1];
                return heap.alloc(DAGNode::new(
                    NodeKind::IR(IRNodeKind::StoreFiOff),
                    vec![op0, op1_op0, op1_op1, new_src],
                    node.ty.clone(),
                ));
            }

            if op0.is_operation()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                let op1_op0 = self.run_on_node(heap, op1.operand[0]);
                let op1_op1 = op1.operand[1];
                return heap.alloc(DAGNode::new(
                    NodeKind::IR(IRNodeKind::StoreRegOff),
                    vec![op0, op1_op0, op1_op1, new_src],
                    node.ty.clone(),
                ));
            }
        }

        self.run_on_node_operand(heap, node);
        node
    }

    fn run_on_node_add(
        &mut self,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].is_frame_index() {
            let op0 = node.operand[0];

            if node.operand[1].is_maybe_register() {
                let op1 = self.run_on_node(heap, node.operand[1]);
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::LEArmr64),
                    vec![op0, op1],
                    node.ty.clone(),
                ));
            } else if node.operand[1].is_constant() {
                let op1 = node.operand[1];
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::LEArmi32),
                    vec![op0, op1],
                    node.ty.clone(),
                ));
            }
            // println!("T {:?}", node.ty);
        }

        self.run_on_node_operand(heap, node);
        node
    }

    fn run_on_node_operand(&mut self, heap: &mut RawAllocator<DAGNode>, mut node: Raw<DAGNode>) {
        node.operand = node
            .operand
            .iter()
            .map(|op| self.run_on_node(heap, *op))
            .collect()
    }
}
