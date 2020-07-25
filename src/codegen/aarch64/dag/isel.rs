use super::super::machine::register::*;
use crate::codegen::arch::dag::node::*;
use crate::codegen::common::dag::{
    function::{DAGFunction, DAGHeap},
    module::DAGModule,
};
use crate::{ir::types::*, traits::pass::ModulePassTrait, util::allocator::*};
use defs::isel_pat;
use rustc_hash::FxHashMap;

impl ModulePassTrait for MISelector {
    type M = DAGModule;

    fn name(&self) -> &'static str {
        "MachineInstSelector"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module);
    }
}

pub struct MISelector {
    selected: FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
}

impl MISelector {
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
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if !node.may_contain_children() {
            return node;
        }

        if let Some(node) = self.selected.get(&node) {
            return *node;
        }

        let mut selected = isel_pat!(
            // TODO: Refactoring
            // (ir.Call _a) => { self.select_call(tys, regs_info, heap, node) }
            // (ir.Add a, b): Int32 {
            //     GPR a {
            //         imm12 b => (mi.ADDIW a, b)
            //         imm32 b => (mi.ADDW  a, (mi.LI b))
            //         GPR   b => (mi.ADDW  a, b) } }
            // (ir.Add a, b) {
            //     GPR a {
            //         imm12 b => (mi.ADDI a, b)
            //         imm32 b => (mi.ADD  a, (mi.LI b))
            //         GPR   b => (mi.ADD  a, b) } }
            // (ir.Mul a, b): Int32 {
            //     GPR a {
            //         imm32 b => (mi.MULW a, (mi.LI b))
            //         GPR   b => (mi.MULW a, b) } }
            // (ir.Mul a, b) {
            //     GPR a {
            //         imm32 b => (mi.MUL a, (mi.LI b))
            //         GPR   b => (mi.MUL a, b) } }
            // (ir.Div a, b): Int32 {
            //     GPR a {
            //         imm32 b => (mi.DIVW a, (mi.LI b))
            //         GPR   b => (mi.DIVW a, b) } }
            // (ir.Rem a, b): Int32 {
            //     GPR a {
            //         imm32 b => (mi.REMW a, (mi.LI b))
            //         GPR   b => (mi.REMW a, b) } }
            // (ir.Br a) => (mi.J a)
            // (ir.Shl a, b) {
            //     GPR a {
            //         imm6 b => (mi.SLLI a, b)
            //     }
            // }
            // (ir.Load a): Int32 {
            //     (ir.FIAddr b) a { mem32 b => (mi.LW [FiReg b, %s0]) }
            //     (ir.GlobalAddr b) a => (mi.LW [Address b])
            //     GPR a => (mi.LW [ImmReg $0, a])
            // }
            // // 64bit load
            // (ir.Load a) { GPR a => (mi.LD [ImmReg $0, a]) }
            // (ir.Store a, b) {
            //     (ir.FIAddr c) a {
            //         mem32 c {
            //             imm32 b => (mi.SW (mi.LI b), [FiReg c, %s0])
            //             GPR   b => (mi.SW         b, [FiReg c, %s0])
            //         }
            //         mem64 c {
            //             GPR b => (mi.SD b, [FiReg c, %s0])
            //         }
            //     }
            //     (ir.GlobalAddr c) a {
            //         imm32 b => (mi.SW (mi.LI b), [Address c], %s1)
            //         GPR   b => (mi.SW         b, [Address c], %s1)
            //     }
            //     GPR a {
            //         imm32 b => (mi.SW (mi.LI b), [ImmReg $0, a])
            //         GPR: Int32 b => (mi.SW b, [ImmReg $0, a])
            //         GPR b => (mi.SD b, [ImmReg $0, a])
            //     }
            // }
            // (ir.FIAddr a) {
            //     mem a => (mi.ADDI %s0, a)
            // }
            // (ir.GlobalAddr a) => (mi.LA [Address a])
            // (ir.CopyFromReg a) => (mi.Copy a)
        );

        self.selected.insert(node, selected);

        if let Some(next) = node.next {
            selected.next = Some(self.run_on_node(tys, regs_info, heap, next));
        }

        selected
    }

    // fn select_call(
    //     &mut self,
    //     tys: &Types,
    //     regs_info: &RegistersInfo,
    //     heap: &mut DAGHeap,
    //     mut node: Raw<DAGNode>,
    // ) -> Raw<DAGNode> {
    //     unimplemented!()
    // const SQRT_F64: &str = "cilk.sqrt.f64";
    // let supported = [SQRT_F64];
    //
    // let name = match &node.operand[0].kind {
    //     NodeKind::Operand(OperandNodeKind::Address(AddressKind::FunctionName(name)))
    //         if supported.contains(&name.as_str()) =>
    //     {
    //         name.as_str()
    //     }
    //     _ => {
    //         node.operand = node
    //             .operand
    //             .iter()
    //             .map(|op| self.run_on_node(tys, regs_info, heap, *op))
    //             .collect();
    //         return node;
    //     }
    // };
    //
    // match name {
    //     SQRT_F64 => {
    //         let x = self.run_on_node(tys, regs_info, heap, node.operand[1]);
    //         heap.alloc(DAGNode::new(
    //             NodeKind::MI(MINodeKind::SQRTSDrr),
    //             vec![x],
    //             Type::F64,
    //         ))
    //     }
    //     _ => unreachable!(),
    // }
    // }
}
