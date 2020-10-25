use super::{
    super::machine::register::*,
    // node::AddressKind,
    node::*,
};
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
            (ir.Call _a) => { self.select_call(tys, regs_info, heap, node) }
            (ir.Add a, b) {
                GR8 a {
                    GR8 b => (mi.ADDrr8 a, b)
                    imm8 b => (mi.ADDri8 a, b)
                }
                GR32 a {
                    GR32  b => (mi.ADDrr32   a, b)
                    imm32 b => (mi.ADDri32   a, b) }
                GR64 a {
                    imm32 b => (mi.ADDr64i32 a, b)
                    GR64  b => (mi.ADDrr64   a, b) }
                XMM a {
                    // (ir.Load c) b {
                    //     (ir.FIAddr d) c {
                    //         f64mem d => (mi.ADDSDrm a, [BaseFi %rbp, d]) } }
                    imm_f64 b => (mi.ADDSDrr a, (mi.MOVSDrm64 b))
                    XMM    b => (mi.ADDSDrr a, b)
                }
            }
            (ir.Sub a, b) {
                GR8 a {
                    GR8 b => (mi.SUBrr8 a, b)
                    imm8 b => (mi.SUBri8 a, b)
                }
                GR32 a {
                    GR32  b => (mi.SUBrr32   a, b)
                    imm32 b => (mi.SUBri32   a, b) }
                imm32 a {
                    GR32 b => (mi.SUBrr32 (mi.MOVri32 a), b)
                }
                GR64 a {
                    imm32 b => (mi.SUBr64i32 a, b) }
                XMM  a {
                    // (ir.Load c) b {
                    //     (ir.FIAddr d) c {
                    //         f64mem d => (mi.SUBSDrm a, [BaseFi %rbp, d]) } }
                    imm_f64 b => (mi.SUBSDrr a, (mi.MOVSDrm64 b))
                    XMM    b => (mi.SUBSDrr a, b)
                }
                imm_f64 a {
                    XMM b => (mi.SUBSDrr (mi.MOVSDrm64 a), b)
                }
            }
            (ir.Mul a, b) {
                GR8 a {
                    GR8  b => (ir.RegClass (mi.IMULrr32 (ir.RegClass a):i32, (ir.RegClass b):i32))
                    imm8 b => (mi.IMULrri8 a, b) }
                GR32 a {
                    GR32  b => (mi.IMULrr32  a, b)
                    imm32 b => (mi.IMULrri32 a, b) }
                GR64 a {
                    imm32 b => (mi.IMULrr64i32 a, b) }
                XMM a {
                    // (ir.Load c) b {
                    //     (ir.FIAddr d) c {
                    //         f64mem d => (mi.MULSDrm a, [BaseFi %rbp, d]) } }
                    imm_f64 b => (mi.MULSDrr a, (mi.MOVSDrm64 b))
                    XMM    b => (mi.MULSDrr a, b)
                }
            }
            (ir.Div a, b) {
                XMM a {
                    (ir.Load c) b {
                        (ir.FIAddr d) c {
                            f64mem d => (mi.DIVSDrm a, [BaseFi %rbp, d]) } }
                    imm_f64 b => (mi.DIVSDrr a, (mi.MOVSDrm64 b))
                    XMM    b => (mi.DIVSDrr a, b)
                }
                imm_f64 a {
                    XMM b => (mi.DIVSDrr (mi.MOVSDrm64 a), b)
                }
            }
            (ir.Shl a, b) {
                GR64 a {
                    imm8 b => (mi.SHLr64i8 a, b) }
                GR32 a {
                    imm8 b => (mi.SHLr32i8 a, b) }
            }
            (ir.SIToFP x): f64 { GR32 x => (mi.CVTSI2SDrr32 x) }
            (ir.FPToSI x): i32 { XMM x => (mi.CVTTSD2SIr32r x) }
            (ir.Sext x): i32 { GR8 x => (mi.MOVSXr32r8 x) }
            (ir.Load a): i8     { (ir.FIAddr     b) a => (mi.MOVrm8  [BaseFi %rbp, b])
                                               GR64 a => (mi.MOVrm8  [Base a]) }
            (ir.Load a): i64    { (ir.FIAddr     b) a => (mi.MOVrm64 [BaseFi %rbp, b])
                                                 GR64 a => (mi.MOVrm64 [Base a]) }
            (ir.Load a): i32    { (ir.FIAddr     b) a => (mi.MOVrm32 [BaseFi %rbp, b])
                                    (ir.GlobalAddr b) a => (mi.MOVrm32 [Address b])
                                                 GR64 a => (mi.MOVrm32 [Base a]) }
            (ir.Load a): f64      { (ir.FIAddr     b) a => (mi.MOVSDrm [BaseFi %rbp, b])
                                                 GR64 a => (mi.MOVSDrm [Base a]) }
            (ir.Load a): Pointer! { (ir.FIAddr     b) a => (mi.MOVrm64 [BaseFi %rbp, b])
                                                 GR64 a => (mi.MOVrm64 [Base a]) }
            (ir.Store a, b) {
                (ir.FIAddr c) a {
                    f64mem c {
                        imm_f64 b => (mi.MOVSDmr [BaseFi %rbp, c], (mi.MOVSDrm64 b))
                    }
                    mem32  c {
                        GR32  b => (mi.MOVmr32 [BaseFi %rbp, c], b)
                        imm32 b => (mi.MOVmi32 [BaseFi %rbp, c], b) }
                    mem64  c {
                        GR64  b => (mi.MOVmr64 [BaseFi %rbp, c], b) }
                    mem c {
                        GR8 b => (mi.MOVmr8 [BaseFi %rbp, c], b)
                        imm8 b => (mi.MOVmi8 [BaseFi %rbp, c], b)
                    }
                }
                (ir.GlobalAddr c) a {
                    imm32 b   => (mi.MOVmi32 [Address c], b)
                    GR32 b    => (mi.MOVmr32 [Address c], b)
                    GR64  b   => (mi.MOVmr64 [Address c], b)
                    imm_f64 b => (mi.MOVSDmr [Address c], (mi.MOVSDrm64 b))
                    XMM    b  => (mi.MOVSDmr [Address c], b)
                }
                GR64   a {
                    imm32 b => (mi.MOVmi32 [Base a], b)
                    GR32  b => (mi.MOVmr32 [Base a], b)
                    GR64  b => (mi.MOVmr64 [Base a], b)
                    imm_f64 b => (mi.MOVSDmr [Base a], (mi.MOVSDrm64 b))
                    XMM    b => (mi.MOVSDmr [Base a], b)
                }
            }
            (ir.FIAddr a) { mem a => (mi.LEAr64m [BaseFi %rbp, a]) }
            // (ir.GlobalAddr a) => (mi.Copy a)  TODO
            (ir.ConstAddr a) => (mi.MOVrm64 [Address a])
            (ir.Br dst) => (mi.JMP dst)
            (ir.CopyFromReg a) => (mi.Copy a)
        );

        self.selected.insert(node, selected);

        if let Some(next) = node.next {
            selected.next = Some(self.run_on_node(tys, regs_info, heap, next));
        }

        selected
    }

    fn select_call(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        heap: &mut DAGHeap,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        const SQRT_F64: &str = "cilk.sqrt.f64";
        let supported = [SQRT_F64];

        let name = match &node.operand[0].kind {
            NodeKind::Operand(OperandNodeKind::Address(AddressKind::FunctionName(name)))
                if supported.contains(&name.as_str()) =>
            {
                name.as_str()
            }
            _ => {
                node.operand = node
                    .operand
                    .iter()
                    .map(|op| self.run_on_node(tys, regs_info, heap, *op))
                    .collect();
                return node;
            }
        };

        match name {
            SQRT_F64 => {
                let x = self.run_on_node(tys, regs_info, heap, node.operand[1]);
                heap.alloc(DAGNode::new(
                    NodeKind::MI(MINodeKind::SQRTSDrr),
                    vec![x],
                    Type::f64,
                ))
            }
            _ => unreachable!(),
        }
    }
}
