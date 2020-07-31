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
        println!("{:?}", module);
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
            (ir.Add a, b) {
                GR64 a {
                    imm32 b => (mi.ADDrr64i a, b)
                    GR64 b => (mi.ADDrrr64 a, b)
                }
            }
            (ir.Add a, b): Int32 {
                GR32 a {
                    imm12 b => (mi.ADDrr32i a, b)
                    imm32 b => (mi.ADDrrr32 a, (mi.MOVr32i b))
                    GR32  b => (mi.ADDrrr32 a, b) } }
            (ir.Sub x, y): Int32 {
                GR32 x {
                    imm12 y => (mi.SUBrr32i x, y)
                    imm32 y => (mi.SUBrrr32 x, (mi.MOVr32i y))
                    GR32  y => (mi.SUBrrr32 x, y) } }
            (ir.Mul x, y): Int32 {
                GR32 x {
                    imm32 y => (mi.MULrrr32 x, (mi.MOVr32i y))
                    GR32  y => (mi.MULrrr32 x, y) } }
            (ir.Mul x, y) {
                GR64 x {
                    imm32 y => (mi.MULrrr64 x, (mi.MOVr32i y))
                    GR64  y => (mi.MULrrr64 x, y) } }
            (ir.Div x, y): Int32 {
                GR32 x {
                    imm32 y => (mi.SDIVrrr32 x, (mi.MOVr32i y))
                    GR32  y => (mi.SDIVrrr32 x, y) } }
            (ir.Br dst) => (mi.B dst)
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
            (ir.Load a): Int32 {
                (ir.FIAddr b) a { mem32 b => (mi.LDR32 [RegFi %x29, b]) }
                GR64 a => (mi.LDR64 [Reg a])
                // (ir.GlobalAddr b) a => (mi.LW [Address b])
                // GPR a => (mi.LW [ImmReg $0, a])
            }
            (ir.Load a) { GR64 a => (mi.LDR64 [Reg a]) }
            // // 64bit load
            // (ir.Load a) { GPR a => (mi.LD [ImmReg $0, a]) }
            (ir.Store a, b) {
                (ir.FIAddr c) a {
                    mem32 c {
                        imm32 b => (mi.STR (mi.MOVr32i b), [RegFi %x29, c])
                        // GPR   b => (mi.SW         b, [FiReg c, %s0])
                    }
                    mem64 c {
                        GR64 b => (mi.STR b, [RegFi %x29, c])
                    }
                }
                GR64 a {
                    imm32 b => (mi.STR (mi.MOVr32i b), [Reg a])
                    GR64 b => (mi.STR b, [Reg a])
                }
                // (ir.GlobalAddr c) a {
                //     imm32 b => (mi.SW (mi.LI b), [Address c], %s1)
                //     GPR   b => (mi.SW         b, [Address c], %s1)
                // }
                // GPR a {
                //     imm32 b => (mi.SW (mi.LI b), [ImmReg $0, a])
                //     GPR: Int32 b => (mi.SW b, [ImmReg $0, a])
                //     GPR b => (mi.SD b, [ImmReg $0, a])
                // }
            }
            (ir.FIAddr a) {
                mem a => (mi.ADDrr64i %x29, a)
            }
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
