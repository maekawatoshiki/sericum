use crate::codegen::arch::dag::node::*;
use crate::codegen::common::dag::{function::*, module::*};
use crate::{ir::types::*, traits::pass::ModulePassTrait, util::allocator::*};
use rustc_hash::FxHashMap;

pub struct Combine {}

impl ModulePassTrait for Combine {
    type M = DAGModule;

    fn name(&self) -> &'static str {
        "Combine"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.combine_module(module);
    }
}

impl Combine {
    pub fn new() -> Self {
        Self {}
    }

    pub fn combine_module(&mut self, module: &mut DAGModule) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
                continue;
            }
            self.combine_function(func)
        }
    }

    fn combine_function(&mut self, func: &mut DAGFunction) {
        for bb_id in &func.dag_basic_blocks {
            let bb = &func.dag_basic_block_arena[*bb_id];
            self.combine_node(
                &mut FxHashMap::default(),
                &mut func.dag_heap,
                bb.entry.unwrap(),
            );
        }
    }

    fn combine_node(
        &mut self,
        replace: &mut FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if !node.may_contain_children() {
            return node;
        }

        if let Some(replaced) = replace.get(&node) {
            return *replaced;
        }

        // TODO: Macro for pattern matching?
        let mut replaced = match &node.kind {
            NodeKind::IR(IRNodeKind::Add) => self.combine_node_add(replace, heap, node),
            NodeKind::IR(IRNodeKind::Mul) => self.combine_node_mul(replace, heap, node),
            NodeKind::IR(IRNodeKind::BrCond) => self.combine_node_brcond(replace, heap, node),
            _ => self.combine_operands(replace, heap, node),
        };

        replace.insert(node, replaced);

        if let Some(next) = node.next {
            replaced.next = Some(self.combine_node(replace, heap, next));
        }

        replaced
    }

    fn combine_node_add(
        &mut self,
        replace: &mut FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        // (C + any) -> (any + C)
        if node.operand[0].is_constant() && !node.operand[1].is_constant() {
            node.operand.swap(0, 1);
        }

        // (~fi + fi) -> (fi + ~fi)
        if node.operand[0].kind != NodeKind::IR(IRNodeKind::FIAddr)
            && node.operand[1].kind == NodeKind::IR(IRNodeKind::FIAddr)
        {
            node.operand.swap(0, 1);
        }

        // (N + 0) -> N
        if node.operand[1].is_constant() && node.operand[1].as_constant().is_null() {
            node.operand[0].ty = node.ty;
            return self.combine_node(replace, heap, node.operand[0]);
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
                NodeKind::Operand(OperandNodeKind::Constant(const_folded)),
                vec![],
                const_folded.get_type(),
            ));
            let new_add = heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::Add),
                vec![op0, c],
                node.ty,
            ));
            return self.combine_node_add(replace, heap, new_add);
        }

        self.combine_operands(replace, heap, node)
    }

    fn combine_node_mul(
        &mut self,
        replace: &mut FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        // (C * !C) -> (!C * C)
        if node.operand[0].is_constant() && !node.operand[1].is_constant() {
            node.operand.swap(0, 1);
        }

        // (N * 0) -> 0
        if node.operand[1].is_constant() && node.operand[1].as_constant().is_null() {
            return node.operand[1];
        }

        // (N * 1) -> N
        if node.operand[1].is_constant() && node.operand[1].as_constant().is_int(1) {
            return node.operand[0];
        }

        self.combine_operands(replace, heap, node)
    }

    fn combine_node_brcond(
        &mut self,
        replace: &mut FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let cond = node.operand[0];
        let br = node.operand[1];
        match cond.kind {
            NodeKind::IR(IRNodeKind::Setcc) | NodeKind::IR(IRNodeKind::FCmp) => {
                let cond_kind = cond.operand[0];
                let lhs = self.combine_node(replace, heap, cond.operand[1]);
                let rhs = self.combine_node(replace, heap, cond.operand[2]);
                heap.alloc(DAGNode::new(
                    match cond.kind {
                        NodeKind::IR(IRNodeKind::Setcc) => NodeKind::IR(IRNodeKind::Brcc),
                        NodeKind::IR(IRNodeKind::FCmp) => NodeKind::IR(IRNodeKind::FPBrcc),
                        _ => unreachable!(),
                    },
                    vec![cond_kind, lhs, rhs, br],
                    Type::Void,
                ))
            }
            _ => self.combine_operands(replace, heap, node),
        }
    }

    fn combine_operands(
        &mut self,
        replace: &mut FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        node.operand = node
            .operand
            .iter()
            .map(|op| self.combine_node(replace, heap, *op))
            .collect();
        node
    }
}
