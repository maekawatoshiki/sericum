use super::super::register::*;
use super::{
    function::{DAGFunction, DAGHeap},
    module::DAGModule,
    node::*,
};
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

    fn run_on_node(&mut self, tys: &Types, heap: &mut DAGHeap, node: Raw<DAGNode>) -> Raw<DAGNode> {
        if !node.may_contain_children() {
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
            NodeKind::IR(IRNodeKind::Mul) => self.run_on_node_mul(tys, heap, node),
            NodeKind::IR(IRNodeKind::Sext) => self.run_on_node_sext(tys, heap, node),
            NodeKind::IR(IRNodeKind::Brcc) => self.run_on_node_brcc(tys, heap, node),
            NodeKind::IR(IRNodeKind::FPBrcc) => self.run_on_node_fpbrcc(tys, heap, node),
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
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].kind == NodeKind::IR(IRNodeKind::Add) {
            let add = node.operand[0];
            let op0 = self.run_on_node(tys, heap, add.operand[0]);
            let op1 = self.run_on_node(tys, heap, add.operand[1]);
            let rbp = heap.alloc_phys_reg(GR64::RBP);

            if op0.kind == NodeKind::IR(IRNodeKind::FIAddr) && op1.is_constant() {
                let mem = heap.alloc(DAGNode::new_mem(
                    MemNodeKind::BaseFiOff,
                    vec![rbp, op0.operand[0], op1],
                ));
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrm32),
                    vec![mem],
                    node.ty.clone(),
                ));
            }

            if op0.kind == NodeKind::IR(IRNodeKind::FIAddr)
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                let op1_op0 = self.run_on_node(tys, heap, op1.operand[0]);
                let op1_op1 = op1.operand[1];
                let mem = heap.alloc(DAGNode::new_mem(
                    MemNodeKind::BaseFiAlignOff,
                    vec![rbp, op0.operand[0], op1_op1, op1_op0],
                ));
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrm32),
                    vec![mem],
                    node.ty.clone(),
                ));
            }

            if op0.is_maybe_register()
                && op1.kind == NodeKind::IR(IRNodeKind::Mul)
                && op1.operand[1].is_constant()
            {
                let op1_op0 = self.run_on_node(tys, heap, op1.operand[0]);
                let op1_op1 = op1.operand[1];
                let mem = heap.alloc(DAGNode::new_mem(
                    MemNodeKind::BaseAlignOff,
                    vec![op0, op1_op1, op1_op0],
                ));
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::MOVrm32),
                    vec![mem],
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
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        isel_pat! {
        (ir.Store dst, src) {
            (ir.Add a1, a2) dst {
                (ir.FIAddr fi) a1 {
                    mem32 fi {
                        imm32 a2 {
                            GR32  src => (mi.MOVmr32 [BaseFiOff %rbp, fi, a2], src)
                            imm32 src => (mi.MOVmi32 [BaseFiOff %rbp, fi, a2], src) }
                        (ir.Mul m1, m2) a2 {
                            imm32 m2 {
                                GR32  src => (mi.MOVmr32 [BaseFiAlignOff %rbp, fi, m2, m1], src)
                                imm32 src => (mi.MOVmi32 [BaseFiAlignOff %rbp, fi, m2, m1], src) } } } }
                GR64 a1 {
                    (ir.Mul m1, m2) a2 {
                        imm32 m2 {
                            GR32  src => (mi.MOVmr32 [BaseAlignOff a1, m2, m1], src)
                            imm32 src => (mi.MOVmi32 [BaseAlignOff a1, m2, m1], src) } } } } } }
    }

    fn run_on_node_add(
        &mut self,
        tys: &Types,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr) {
            let op0 = node.operand[0].operand[0];
            let rbp = heap.alloc_phys_reg(GR64::RBP);
            let one = heap.alloc(DAGNode::new(
                NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(1))),
                vec![],
                Type::Int32,
            ));

            if node.operand[1].is_maybe_register() {
                let op1 = self.run_on_node(tys, heap, node.operand[1]);
                let mem = heap.alloc(DAGNode::new_mem(
                    MemNodeKind::BaseFiAlignOff,
                    vec![rbp, op0, one, op1],
                ));
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::LEAr64m),
                    vec![mem],
                    node.ty.clone(),
                ));
            } else if node.operand[1].is_constant() {
                let op1 = node.operand[1];
                let mem = heap.alloc(DAGNode::new_mem(
                    MemNodeKind::BaseFiOff,
                    vec![rbp, op0, op1],
                ));
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::LEAr64m),
                    vec![mem],
                    node.ty.clone(),
                ));
            }
        }

        self.run_on_node_operand(tys, heap, node);
        node
    }

    fn run_on_node_mul(
        &mut self,
        tys: &Types,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let mut lhs = node.operand[0];
        let mut rhs = node.operand[1];
        if lhs.is_constant() && rhs.is_maybe_register() {
            ::std::mem::swap(&mut *lhs, &mut *rhs);
        }

        self.run_on_node_operand(tys, heap, node);
        node
    }

    fn run_on_node_sext(
        &mut self,
        tys: &Types,
        heap: &mut DAGHeap,
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

        // TODO: Need instruction that converts Int32 to Int64
        if node.ty == Type::Int64 && node.operand[0].ty == Type::Int32
        // && node.operand[0].operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr)
        {
            return self.run_on_node(tys, heap, node.operand[0]);
            // return node
        }

        self.run_on_node_operand(tys, heap, node);
        node
    }

    fn run_on_node_brcc(
        &mut self,
        tys: &Types,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let mut cond = node.operand[0];
        let mut lhs = node.operand[1];
        let mut rhs = node.operand[2];
        // lhs must be register
        if lhs.is_constant() && rhs.is_maybe_register() {
            ::std::mem::swap(&mut *lhs, &mut *rhs);
            if let NodeKind::Operand(OperandNodeKind::CondKind(kind)) = &mut cond.kind {
                *kind = kind.flip();
            }
        }
        self.run_on_node_operand(tys, heap, node);
        node
    }

    fn run_on_node_fpbrcc(
        &mut self,
        tys: &Types,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let mut cond = node.operand[0];
        let mut lhs = node.operand[1];
        let mut rhs = node.operand[2];

        // lhs must be register
        if lhs.is_constant() && rhs.is_maybe_register() {
            ::std::mem::swap(&mut *lhs, &mut *rhs);
            if let NodeKind::Operand(OperandNodeKind::CondKind(kind)) = &mut cond.kind {
                *kind = kind.flip();
            }
        }

        // now: lhs=register, rhs=const|register
        if lhs.is_maybe_register() && rhs.is_constant() {
            let lhs = self.run_on_node(tys, heap, lhs);
            let rhs = heap.alloc(DAGNode::new(
                NodeKind::MI(MINodeKind::MOVSDrm64),
                vec![rhs],
                Type::F64,
            ));
            return heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::FPBrcc),
                vec![cond, lhs, rhs, node.operand[3]],
                node.ty.clone(),
            ));
        }

        self.run_on_node_operand(tys, heap, node);
        node
    }

    fn run_on_node_operand(&mut self, tys: &Types, heap: &mut DAGHeap, mut node: Raw<DAGNode>) {
        node.operand = node
            .operand
            .iter()
            .map(|op| self.run_on_node(tys, heap, *op))
            .collect()
    }
}
