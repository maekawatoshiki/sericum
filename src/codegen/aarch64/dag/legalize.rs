use super::super::machine::register::*;
use crate::codegen::arch::dag::node::*;
use crate::codegen::common::dag::{
    function::{DAGFunction, DAGHeap},
    module::DAGModule,
};
use crate::{ir::types::*, traits::pass::ModulePassTrait, util::allocator::*};
// use defs::isel_pat;
use rustc_hash::FxHashMap;

impl ModulePassTrait for Legalize {
    type M = DAGModule;

    fn name(&self) -> &'static str {
        "Legalize"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module);
    }
}

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
            if func.is_internal {
                continue;
            }
            self.run_on_function(&module.types, func)
        }
    }

    fn run_on_function(&mut self, tys: &Types, func: &mut DAGFunction) {
        for bb_id in &func.dag_basic_blocks {
            let bb = &func.dag_basic_block_arena[*bb_id];
            self.run_on_node(tys, &func.regs_info, &mut func.dag_heap, bb.entry.unwrap());
        }
    }

    fn run_on_node(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if !node.may_contain_children() {
            return node;
        }

        if let Some(node) = self.selected.get(&node) {
            return *node;
        }

        // TODO: auto-generate the following code by macro
        let mut selected = match node.kind {
            // NodeKind::IR(IRNodeKind::Load) => self.run_on_node_load(tys, regs_info, heap, node),
            // NodeKind::IR(IRNodeKind::Store) => self.run_on_node_store(tys, regs_info, heap, node),
            // NodeKind::IR(IRNodeKind::Add) => self.run_on_node_add(tys, regs_info, heap, node),
            // NodeKind::IR(IRNodeKind::Mul) => self.run_on_node_mul(tys, regs_info, heap, node),
            NodeKind::IR(IRNodeKind::Sext) => self.run_on_node_sext(tys, regs_info, heap, node),
            // NodeKind::IR(IRNodeKind::Brcc) => self.run_on_node_brcc(tys, regs_info, heap, node),
            // NodeKind::IR(IRNodeKind::FPBrcc) => self.run_on_node_fpbrcc(tys, regs_info, heap, node),
            NodeKind::IR(IRNodeKind::Sub) => self.run_on_node_sub(tys, regs_info, heap, node),
            _ => {
                self.run_on_node_operand(tys, regs_info, heap, node);
                node
            }
        };

        self.selected.insert(node, selected);

        if let Some(next) = node.next {
            selected.next = Some(self.run_on_node(tys, regs_info, heap, next));
        }

        selected
    }

    fn run_on_node_sub(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.kind == NodeKind::IR(IRNodeKind::Sub)
            && !node.operand[0].is_constant()
            && node.operand[1].is_constant()
        {
            // TODO: Refine code
            let op0 = self.run_on_node(tys, regs_info, heap, node.operand[0]);
            let op1 = heap.alloc(DAGNode::new(
                NodeKind::Operand(OperandNodeKind::Constant(
                    node.operand[1].as_constant().neg(),
                )),
                vec![],
                node.operand[1].ty,
            ));
            return heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::Add),
                vec![op0, op1],
                node.ty,
            ));
        }

        self.run_on_node_operand(tys, regs_info, heap, node);
        node
    }

    // fn run_on_node_load(
    //     &mut self,
    //     tys: &Types,
    //     regs_info: &RegistersInfo,
    //     heap: &mut DAGHeap,
    //     node: Raw<DAGNode>,
    // ) -> Raw<DAGNode> {
    //     if matches!(node.ty, Type::Int32) && node.operand[0].kind == NodeKind::IR(IRNodeKind::Add) {
    //         let add = node.operand[0];
    //         let op0 = self.run_on_node(tys, regs_info, heap, add.operand[0]);
    //         let op1 = self.run_on_node(tys, regs_info, heap, add.operand[1]);
    //         let rbp = heap.alloc_phys_reg(regs_info, GR64::RBP);
    //
    //         if op0.kind == NodeKind::IR(IRNodeKind::FIAddr) && op1.is_constant() {
    //             let mem = heap.alloc(DAGNode::new_mem(
    //                 MemNodeKind::BaseFiOff,
    //                 vec![rbp, op0.operand[0], op1],
    //             ));
    //             return heap.alloc(DAGNode::new(
    //                 NodeKind::MI(MINodeKind::MOVrm32),
    //                 vec![mem],
    //                 node.ty.clone(),
    //             ));
    //         }
    //
    //         if op0.kind == NodeKind::IR(IRNodeKind::FIAddr)
    //             && op1.kind == NodeKind::IR(IRNodeKind::Mul)
    //             && op1.operand[1].is_constant()
    //         {
    //             let op1_op0 = self.run_on_node(tys, regs_info, heap, op1.operand[0]);
    //             let op1_op1 = op1.operand[1];
    //             let mem = heap.alloc(DAGNode::new_mem(
    //                 MemNodeKind::BaseFiAlignOff,
    //                 vec![rbp, op0.operand[0], op1_op1, op1_op0],
    //             ));
    //             return heap.alloc(DAGNode::new(
    //                 NodeKind::MI(MINodeKind::MOVrm32),
    //                 vec![mem],
    //                 node.ty,
    //             ));
    //         }
    //
    //         if op0.is_maybe_register()
    //             && op1.kind == NodeKind::IR(IRNodeKind::Mul)
    //             && op1.operand[1].is_constant()
    //         {
    //             let op1_op0 = self.run_on_node(tys, regs_info, heap, op1.operand[0]);
    //             let op1_op1 = op1.operand[1];
    //             let mem = heap.alloc(DAGNode::new_mem(
    //                 MemNodeKind::BaseAlignOff,
    //                 vec![op0, op1_op1, op1_op0],
    //             ));
    //             return heap.alloc(DAGNode::new(
    //                 NodeKind::MI(MINodeKind::MOVrm32),
    //                 vec![mem],
    //                 node.ty.clone(),
    //             ));
    //         }
    //     }
    //
    //     self.run_on_node_operand(tys, regs_info, heap, node);
    //     node
    // }
    //
    // fn run_on_node_store(
    //     &mut self,
    //     tys: &Types,
    //     regs_info: &RegistersInfo,
    //     heap: &mut DAGHeap,
    //     mut node: Raw<DAGNode>,
    // ) -> Raw<DAGNode> {
    //     isel_pat! {
    //     (ir.Store dst, src) {
    //         (ir.Add a1, a2) dst {
    //             (ir.FIAddr fi) a1 {
    //                 mem32 fi {
    //                     imm32 a2 {
    //                         GR32  src => (mi.MOVmr32 [BaseFiOff %rbp, fi, a2], src)
    //                         imm32 src => (mi.MOVmi32 [BaseFiOff %rbp, fi, a2], src) }
    //                     (ir.Mul m1, m2) a2 {
    //                         imm32 m2 {
    //                             GR32  src => (mi.MOVmr32 [BaseFiAlignOff %rbp, fi, m2, m1], src)
    //                             imm32 src => (mi.MOVmi32 [BaseFiAlignOff %rbp, fi, m2, m1], src) } } } }
    //             GR64 a1 {
    //                 (ir.Mul m1, m2) a2 {
    //                     imm32 m2 {
    //                         GR32  src => (mi.MOVmr32 [BaseAlignOff a1, m2, m1], src)
    //                         imm32 src => (mi.MOVmi32 [BaseAlignOff a1, m2, m1], src) } } } } } }
    // }
    //
    // fn run_on_node_add(
    //     &mut self,
    //     tys: &Types,
    //     regs_info: &RegistersInfo,
    //     heap: &mut DAGHeap,
    //     node: Raw<DAGNode>,
    // ) -> Raw<DAGNode> {
    //     if node.operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr) {
    //         let op0 = node.operand[0].operand[0];
    //         let rbp = heap.alloc_phys_reg(regs_info, GR64::RBP);
    //         let one = heap.alloc(DAGNode::new(
    //             NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(1))),
    //             vec![],
    //             Type::Int32,
    //         ));
    //
    //         if node.operand[1].is_maybe_register() {
    //             let op1 = self.run_on_node(tys, regs_info, heap, node.operand[1]);
    //             let mem = heap.alloc(DAGNode::new_mem(
    //                 MemNodeKind::BaseFiAlignOff,
    //                 vec![rbp, op0, one, op1],
    //             ));
    //             return heap.alloc(DAGNode::new(
    //                 NodeKind::MI(MINodeKind::LEAr64m),
    //                 vec![mem],
    //                 node.ty,
    //             ));
    //         } else if node.operand[1].is_constant() {
    //             let op1 = node.operand[1];
    //             let mem = heap.alloc(DAGNode::new_mem(
    //                 MemNodeKind::BaseFiOff,
    //                 vec![rbp, op0, op1],
    //             ));
    //             return heap.alloc(DAGNode::new(
    //                 NodeKind::MI(MINodeKind::LEAr64m),
    //                 vec![mem],
    //                 node.ty,
    //             ));
    //         }
    //     }
    //
    //     self.run_on_node_operand(tys, regs_info, heap, node);
    //     node
    // }
    //
    // fn run_on_node_mul(
    //     &mut self,
    //     tys: &Types,
    //     regs_info: &RegistersInfo,
    //     heap: &mut DAGHeap,
    //     mut node: Raw<DAGNode>,
    // ) -> Raw<DAGNode> {
    //     let lhs = node.operand[0];
    //     let rhs = node.operand[1];
    //
    //     if lhs.is_constant() && rhs.is_maybe_register() {
    //         let tmp = node.operand[0];
    //         node.operand[0] = node.operand[1];
    //         node.operand[0] = tmp;
    //         return self.run_on_node_mul(tys, regs_info, heap, node);
    //     }
    //
    //     self.run_on_node_operand(tys, regs_info, heap, node);
    //     node
    // }
    //
    fn run_on_node_sext(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        // if node.ty == Type::Int64
        //     && node.operand[0].kind == NodeKind::IR(IRNodeKind::Load)
        //     && node.operand[0].operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr)
        // {
        //     return heap.alloc(DAGNode::new(
        //         NodeKind::MI(MINodeKind::MOVSXDr64m32),
        //         vec![node.operand[0].operand[0].operand[0]],
        //         node.ty.clone(),
        //     ));
        // }

        if node.ty == Type::Int64
            && !node.operand[0].is_constant()
            && node.operand[0].ty == Type::Int32
        {
            let op = self.run_on_node(tys, regs_info, heap, node.operand[0]);
            return heap.alloc(DAGNode::new(
                NodeKind::MI(MINodeKind::SEXT_W),
                vec![op],
                node.ty,
            ));
        }

        self.run_on_node_operand(tys, regs_info, heap, node);
        node
    }
    //
    // fn run_on_node_brcc(
    //     &mut self,
    //     tys: &Types,
    //     regs_info: &RegistersInfo,
    //     heap: &mut DAGHeap,
    //     mut node: Raw<DAGNode>,
    // ) -> Raw<DAGNode> {
    //     let mut cond = node.operand[0];
    //     let lhs = node.operand[1];
    //     let rhs = node.operand[2];
    //     // lhs must be register
    //     if lhs.is_constant() && rhs.is_maybe_register() {
    //         let tmp = node.operand[1];
    //         node.operand[1] = node.operand[2];
    //         node.operand[2] = tmp;
    //         if let NodeKind::Operand(OperandNodeKind::CondKind(kind)) = &mut cond.kind {
    //             *kind = kind.flip();
    //         }
    //         return self.run_on_node_brcc(tys, regs_info, heap, node);
    //     }
    //     self.run_on_node_operand(tys, regs_info, heap, node);
    //     node
    // }
    //
    // fn run_on_node_fpbrcc(
    //     &mut self,
    //     tys: &Types,
    //     regs_info: &RegistersInfo,
    //     heap: &mut DAGHeap,
    //     mut node: Raw<DAGNode>,
    // ) -> Raw<DAGNode> {
    //     let mut cond = node.operand[0];
    //     let lhs = node.operand[1];
    //     let rhs = node.operand[2];
    //
    //     // lhs must be register
    //     if lhs.is_constant() && rhs.is_maybe_register() {
    //         let tmp = lhs;
    //         node.operand[1] = node.operand[2];
    //         node.operand[2] = tmp;
    //         if let NodeKind::Operand(OperandNodeKind::CondKind(kind)) = &mut cond.kind {
    //             *kind = kind.flip();
    //         }
    //         return self.run_on_node_fpbrcc(tys, regs_info, heap, node);
    //     }
    //
    //     // now: lhs=register, rhs=const|register
    //     if lhs.is_maybe_register() && rhs.is_constant() {
    //         let lhs = self.run_on_node(tys, regs_info, heap, lhs);
    //         let rhs = heap.alloc(DAGNode::new(
    //             NodeKind::MI(MINodeKind::MOVSDrm64),
    //             vec![rhs],
    //             Type::F64,
    //         ));
    //         return heap.alloc(DAGNode::new(
    //             NodeKind::IR(IRNodeKind::FPBrcc),
    //             vec![cond, lhs, rhs, node.operand[3]],
    //             node.ty,
    //         ));
    //     }
    //
    //     self.run_on_node_operand(tys, regs_info, heap, node);
    //     node
    // }

    fn run_on_node_operand(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) {
        node.operand = node
            .operand
            .iter()
            .map(|op| self.run_on_node(tys, regs_info, heap, *op))
            .collect()
    }
}
