use super::{super::machine::register::*, node::*};
use crate::codegen::common::dag::{
    function::{DAGFunction, DAGHeap},
    module::DAGModule,
};
use crate::{ir::types::*, traits::pass::ModulePassTrait, util::allocator::*};
use defs::isel_pat;
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
            NodeKind::IR(IRNodeKind::Load) => self.run_on_node_load(tys, regs_info, heap, node),
            NodeKind::IR(IRNodeKind::Store) => self.run_on_node_store(tys, regs_info, heap, node),
            NodeKind::IR(IRNodeKind::Add) => self.run_on_node_add(tys, regs_info, heap, node),
            NodeKind::IR(IRNodeKind::Mul) => self.run_on_node_mul(tys, regs_info, heap, node),
            NodeKind::IR(IRNodeKind::Sext) => self.run_on_node_sext(tys, regs_info, heap, node),
            NodeKind::IR(IRNodeKind::Brcc) => self.run_on_node_brcc(tys, regs_info, heap, node),
            NodeKind::IR(IRNodeKind::FPBrcc) => self.run_on_node_fpbrcc(tys, regs_info, heap, node),
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

    fn run_on_node_load(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        isel_pat! {
        (ir.Load dst): i32 {
            (ir.Add x, y) dst {
                (ir.FIAddr fi) x {
                    (ir.Mul z, u) y {
                        imm32 u {
                            (ir.Sext q) z {
                                (ir.Add w, e) q {
                                    imm32 e => { {
                                        // TODO: Refactoring
                                        let fi = self.run_on_node(tys, regs_info, heap, fi);
                                        let w = self.run_on_node(tys, regs_info, heap, w);
                                        let rbp = heap.alloc_phys_reg(regs_info, GR64::RBP);
                                        let off = e.kind.as_operand().as_constant().as_i32()
                                            * u.kind.as_operand().as_constant().as_i32();
                                        let off = heap.alloc(DAGNode::new(
                                            NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(off))),
                                            vec![],
                                            Type::i32
                                        ));
                                        let m = heap.alloc(DAGNode::new_mem(
                                            MemNodeKind::BaseFiAlignOffOff,
                                            vec![rbp, fi, u, w, off],
                                        ));
                                        heap.alloc(DAGNode::new(
                                            NodeKind::MI(MINodeKind::MOVrm32),
                                            vec![m],
                                            Type::i32
                                        ))
                                        // mov [rbp + fi + w*u + e * u]
                                    } }
                                }
                            }
                        }
                    }
                }
            }

            (ir.Add x, y) dst {
                (ir.FIAddr fi) x {
                    imm32 y => (mi.MOVrm32 [BaseFiOff %rbp, fi, y])
                    (ir.Mul z, u) y {
                        imm32 u => (mi.MOVrm32 [BaseFiAlignOff %rbp, fi, u, z]) } }
                (ir.GlobalAddr g) x {
                    imm32 y => (mi.MOVrm32 [AddressOff g, y])
                    (ir.Mul z, u) y {
                        imm32 u => (mi.MOVrm32 [AddressAlignOff g, u, z]) } }
                GR64 x {
                    (ir.Mul z, u) y {
                        imm32 u => (mi.MOVrm32 [BaseAlignOff x, u, (ir.RegClass z):i64]) } }
            }
        }
        (ir.Load dst): i8 {
            (ir.Add x, y) dst {
                (ir.FIAddr fi) x {
                    imm32 y => (mi.MOVrm8 [BaseFiOff %rbp, fi, y])
                    (ir.Mul z, u) y {
                        imm32 u => (mi.MOVrm8 [BaseFiAlignOff %rbp, fi, u, z]) } }
                (ir.GlobalAddr g) x {
                    imm32 y => (mi.MOVrm8 [AddressOff g, y])
                    (ir.Mul z, u) y {
                        imm32 u => (mi.MOVrm8 [AddressAlignOff g, u, z]) } }
                GR64 x {
                    imm32 y => (mi.MOVrm8 [BaseOff x, y])
                    GR64  y => (mi.MOVrm8 [BaseAlignOff x, $1, y]) } } }
        (ir.Load dst): f64 {
            (ir.Add x, y) dst {
                (ir.FIAddr fi) x {
                    imm32 y => (mi.MOVSDrm [BaseFiOff %rbp, fi, y])
                    (ir.Mul z, u) y {
                        imm32 u => (mi.MOVSDrm [BaseFiAlignOff %rbp, fi, u, z]) } } } }
        }
    }

    fn run_on_node_store(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        isel_pat! {
        (ir.Store dst, src) {
            (ir.FIAddr fi) dst {
                mem fi {
                    imm32   src => (mi.MOVmi32 [BaseFi %rbp, fi], src)
                    GR32    src => (mi.MOVmr32 [BaseFi %rbp, fi], src)
                    GR64    src => (mi.MOVmr64 [BaseFi %rbp, fi], src)
                    XMM     src => (mi.MOVSDmr [BaseFi %rbp, fi], src)
                    imm_f64 src => (mi.MOVSDmr [BaseFi %rbp, fi], (mi.MOVSDrm64 src)) } }
            (ir.Add a1, a2) dst {
                (ir.FIAddr fi) a1 {
                    mem fi {
                        imm32 a2 {
                            GR32    src => (mi.MOVmr32 [BaseFiOff %rbp, fi, a2], src)
                            GR64    src => (mi.MOVmr64 [BaseFiOff %rbp, fi, a2], src)
                            imm32   src => (mi.MOVmi32 [BaseFiOff %rbp, fi, a2], src)
                            XMM     src => (mi.MOVSDmr [BaseFiOff %rbp, fi, a2], src)
                            imm_f64 src => (mi.MOVSDmr [BaseFiOff %rbp, fi, a2], (mi.MOVSDrm64 src)) } }
                    mem fi {
                        imm32 a2 {
                            GR32  src => (mi.MOVmr32 [BaseFiOff %rbp, fi, a2], src)
                            imm32 src => (mi.MOVmi32 [BaseFiOff %rbp, fi, a2], src) }
                        (ir.Mul m1, m2) a2 {
                            imm32 m2 {
                                (ir.Sext q) m1 {
                                    (ir.Add w, e) q {
                                        imm32 e => { {
                                            // TODO: Refactoring
                                            let fi = self.run_on_node(tys, regs_info, heap, fi);
                                            let w = self.run_on_node(tys, regs_info, heap, w);
                                            let rbp = heap.alloc_phys_reg(regs_info, GR64::RBP);
                                            let off = e.kind.as_operand().as_constant().as_i32()
                                                * m2.kind.as_operand().as_constant().as_i32();
                                            let off = heap.alloc(DAGNode::new(
                                                NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(off))),
                                                vec![],
                                                Type::i32
                                            ));
                                            let m = heap.alloc(DAGNode::new_mem(
                                                MemNodeKind::BaseFiAlignOffOff,
                                                vec![rbp, fi, m2, w, off],
                                            ));
                                            heap.alloc(DAGNode::new(
                                                NodeKind::MI(MINodeKind::MOVmr32),
                                                vec![m, src],
                                                Type::Void
                                            ))
                                        } }
                                    }
                                }
                                GR32  src => (mi.MOVmr32 [BaseFiAlignOff %rbp, fi, m2, m1], src)
                                imm32 src => (mi.MOVmi32 [BaseFiAlignOff %rbp, fi, m2, m1], src)
                            }
                        } } }
                (ir.GlobalAddr g) a1 {
                    imm32 a2 {
                        GR32  src => (mi.MOVmr32 [AddressOff g, a2], src)
                        imm32 src => (mi.MOVmi32 [AddressOff g, a2], src) }
                    (ir.Mul m1, m2) a2 {
                        imm32 m2 {
                            GR32  src => (mi.MOVmr32 [AddressAlignOff g, m2, m1], src)
                            imm32 src => (mi.MOVmi32 [AddressAlignOff g, m2, m1], src) } } }
                GR64 a1 {
                    (ir.Mul m1, m2) a2 {
                        imm32 m2 {
                            GR32  src => (mi.MOVmr32 [BaseAlignOff a1, m2, (ir.RegClass m1):i64], src)
                            imm32 src => (mi.MOVmi32 [BaseAlignOff a1, m2, (ir.RegClass m1):i64], src) } } } } } }
    }

    fn run_on_node_add(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr) {
            let op0 = node.operand[0].operand[0];
            let rbp = heap.alloc_phys_reg(regs_info, GR64::RBP);
            let one = heap.alloc(DAGNode::new(
                NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(1))),
                vec![],
                Type::i32,
            ));

            if node.operand[1].is_maybe_register() {
                let op1 = self.run_on_node(tys, regs_info, heap, node.operand[1]);
                let mem = heap.alloc(DAGNode::new_mem(
                    MemNodeKind::BaseFiAlignOff,
                    vec![rbp, op0, one, op1],
                ));
                return heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::LEAr64m),
                    vec![mem],
                    node.ty,
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
                    node.ty,
                ));
            }
        }

        self.run_on_node_operand(tys, regs_info, heap, node);
        node
    }

    fn run_on_node_mul(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let lhs = node.operand[0];
        let rhs = node.operand[1];

        if lhs.is_constant() && rhs.is_maybe_register() {
            let tmp = node.operand[0];
            node.operand[0] = node.operand[1];
            node.operand[0] = tmp;
            return self.run_on_node_mul(tys, regs_info, heap, node);
        }

        self.run_on_node_operand(tys, regs_info, heap, node);
        node
    }

    fn run_on_node_sext(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if node.ty == Type::i64
            && node.operand[0].kind == NodeKind::IR(IRNodeKind::Load)
            && node.operand[0].ty == Type::i32
            && node.operand[0].operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr)
        {
            let rbp = heap.alloc_phys_reg(regs_info, GR64::RBP);
            let mem = heap.alloc(DAGNode::new_mem(
                MemNodeKind::BaseFi,
                vec![rbp, node.operand[0].operand[0].operand[0]],
            ));
            return heap.alloc(DAGNode::new(
                NodeKind::MI(MINodeKind::MOVSXDr64m32),
                vec![mem],
                node.ty.clone(),
            ));
        }

        // TODO: Need instruction that converts i32 to i64
        if node.ty == Type::i64 && !node.operand[0].is_constant() && node.operand[0].ty == Type::i32
        // && node.operand[0].operand[0].kind == NodeKind::IR(IRNodeKind::FIAddr)
        {
            let op = self.run_on_node(tys, regs_info, heap, node.operand[0]);
            return heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::RegClass),
                vec![op],
                node.ty,
            ));
        }

        self.run_on_node_operand(tys, regs_info, heap, node);
        node
    }

    fn run_on_node_brcc(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let mut cond = node.operand[0];
        let lhs = node.operand[1];
        let rhs = node.operand[2];
        // lhs must be register
        if lhs.is_constant() && rhs.is_maybe_register() {
            let tmp = node.operand[1];
            node.operand[1] = node.operand[2];
            node.operand[2] = tmp;
            if let NodeKind::Operand(OperandNodeKind::CondKind(kind)) = &mut cond.kind {
                *kind = kind.flip();
            }
            return self.run_on_node_brcc(tys, regs_info, heap, node);
        }
        self.run_on_node_operand(tys, regs_info, heap, node);
        node
    }

    fn run_on_node_fpbrcc(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let mut cond = node.operand[0];
        let lhs = node.operand[1];
        let rhs = node.operand[2];

        // lhs must be register
        if lhs.is_constant() && rhs.is_maybe_register() {
            let tmp = lhs;
            node.operand[1] = node.operand[2];
            node.operand[2] = tmp;
            if let NodeKind::Operand(OperandNodeKind::CondKind(kind)) = &mut cond.kind {
                *kind = kind.flip();
            }
            return self.run_on_node_fpbrcc(tys, regs_info, heap, node);
        }

        // now: lhs=register, rhs=const|register
        if lhs.is_maybe_register() && rhs.is_constant() {
            let lhs = self.run_on_node(tys, regs_info, heap, lhs);
            let rhs = heap.alloc(DAGNode::new(
                NodeKind::MI(MINodeKind::MOVSDrm64),
                vec![rhs],
                Type::f64,
            ));
            return heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::FPBrcc),
                vec![cond, lhs, rhs, node.operand[3]],
                node.ty,
            ));
        }

        self.run_on_node_operand(tys, regs_info, heap, node);
        node
    }

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
