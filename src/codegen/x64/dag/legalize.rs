use super::super::register::*;
use super::{function::DAGFunction, module::DAGModule, node::*};
use crate::ir::types::*;
use crate::util::allocator::*;
use defs::isel_pat;
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
            self.run_on_function(&module.types, func)
        }
    }

    fn run_on_function(&mut self, tys: &Types, func: &mut DAGFunction) {
        for bb_id in &func.dag_basic_blocks {
            let bb = &func.dag_basic_block_arena[*bb_id];
            self.run_on_node(tys, &mut func.dag_heap, bb.entry.unwrap());
        }
    }

    fn run_on_node(
        &mut self,
        tys: &Types,
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
            NodeKind::IR(IRNodeKind::Load) => self.run_on_node_load(tys, heap, node),
            NodeKind::IR(IRNodeKind::Store) => self.run_on_node_store(tys, heap, node),
            NodeKind::IR(IRNodeKind::Add) => self.run_on_node_add(tys, heap, node),
            NodeKind::IR(IRNodeKind::Sext) => self.run_on_node_sext(tys, heap, node),
            _ => {
                self.run_on_node_operand(tys, heap, node);
                node
            }
        };

        self.selected.insert(node, selected);

        if let Some(next) = node.next {
            selected.next = Some(self.run_on_node(tys, heap, next));
        }

        selected
    }

    fn run_on_node_load(
        &mut self,
        tys: &Types,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].kind == NodeKind::IR(IRNodeKind::Add) {
            let add = node.operand[0];
            let op0 = self.run_on_node(tys, heap, add.operand[0]);
            let op1 = self.run_on_node(tys, heap, add.operand[1]);
            let none = heap.alloc(DAGNode::new_none());
            let rbp = heap.alloc(DAGNode::new_phys_reg(GR64::RSP));

            if op0.kind == NodeKind::IR(IRNodeKind::FIAddr) && op1.is_constant() {
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrm32),
                    vec![rbp, op0.operand[0], none, op1],
                    node.ty.clone(),
                ));
            }

            if op0.kind == NodeKind::IR(IRNodeKind::FIAddr)
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                let op1_op0 = self.run_on_node(tys, heap, op1.operand[0]);
                let op1_op1 = op1.operand[1];
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrm32),
                    vec![rbp, op0.operand[0], op1_op1, op1_op0],
                    node.ty.clone(),
                ));
            }

            if op0.is_maybe_register()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                let op1_op0 = self.run_on_node(tys, heap, op1.operand[0]);
                let op1_op1 = op1.operand[1];
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrm32),
                    vec![op0, none, op1_op1, op1_op0],
                    node.ty.clone(),
                ));
            }
        }

        self.run_on_node_operand(tys, heap, node);
        node
    }

    fn run_on_node_store(
        &mut self,
        tys: &Types,
        heap: &mut RawAllocator<DAGNode>,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        isel_pat! {
        (ir.Store dst, src) {
            (ir.Add a1, a2) dst {
                (ir.FIAddr fi) a1 {
                    mem32 fi {
                        imm32 a2 {
                            GR32  src => (mi.MOVmr32 %rbp, fi, none, a2, src)
                            imm32 src => (mi.MOVmi32 %rbp, fi, none, a2, src) }
                        (ir.Mul m1, m2) a2 {
                            imm32 m2 {
                                GR32  src => (mi.MOVmr32 %rbp, fi, m2, m1, src)
                                imm32 src => (mi.MOVmi32 %rbp, fi, m2, m1, src) } } } }
                GR64 a1 {
                    (ir.Mul m1, m2) a2 {
                        imm32 m2 {
                            GR32  src => (mi.MOVmr32 a1, none, m2, m1, src)
                            imm32 src => (mi.MOVmi32 a1, none, m2, m1, src) } } } } } }
    }

    fn run_on_node_add(
        &mut self,
        tys: &Types,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr) {
            let op0 = node.operand[0].operand[0];
            let none = heap.alloc(DAGNode::new_none());
            let rbp = heap.alloc(DAGNode::new_phys_reg(GR64::RSP));
            let one = heap.alloc(DAGNode::new(
                NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(1))),
                vec![],
                Type::Int32,
            ));

            if node.operand[1].is_maybe_register() {
                let op1 = self.run_on_node(tys, heap, node.operand[1]);
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::LEAr64m),
                    vec![rbp, op0, one, op1],
                    node.ty.clone(),
                ));
            } else if node.operand[1].is_constant() {
                let op1 = node.operand[1];
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::LEAr64m),
                    vec![rbp, op0, none, op1],
                    node.ty.clone(),
                ));
            }
        }

        self.run_on_node_operand(tys, heap, node);
        node
    }

    fn run_on_node_sext(
        &mut self,
        tys: &Types,
        heap: &mut RawAllocator<DAGNode>,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.ty == Type::Int64
            && node.operand[0].kind == NodeKind::IR(IRNodeKind::Load)
            && node.operand[0].operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr)
        {
            return heap.alloc(DAGNode::new(
                NodeKind::MI(MINodeKind::MOVSXDr64m32),
                vec![node.operand[0].operand[0].operand[0]],
                node.ty.clone(),
            ));
        }

        self.run_on_node_operand(tys, heap, node);
        node
    }

    fn run_on_node_operand(
        &mut self,
        tys: &Types,
        heap: &mut RawAllocator<DAGNode>,
        mut node: Raw<DAGNode>,
    ) {
        node.operand = node
            .operand
            .iter()
            .map(|op| self.run_on_node(tys, heap, *op))
            .collect()
    }
}
