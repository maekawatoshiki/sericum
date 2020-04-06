use super::super::register::*;
use super::{function::DAGFunction, module::DAGModule, node::*};
use crate::ir::types::*;
use crate::util::allocator::*;
use defs::isel_pat;
use rustc_hash::FxHashMap;

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
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        if !node.is_operation() {
            return node;
        }

        if let Some(node) = self.selected.get(&node) {
            return *node;
        }

        let mut selected = isel_pat!(
            (ir.Add a, b) {
                GR32 a {
                    GR32  b => (mi.ADDrr32   a, b)
                    imm32 b => (mi.ADDri32   a, b) }
                GR64 a {
                    imm32 b => (mi.ADDr64i32 a, b)
                    GR64  b => (mi.ADDrr64   a, b) } }
            (ir.Sub a, b) {
                GR32 a {
                    GR32  b => (mi.SUBrr32   a, b)
                    imm32 b => (mi.SUBri32   a, b) }
                GR64 a {
                    imm32 b => (mi.SUBr64i32 a, b) } }
            (ir.Mul a, b) {
                GR32 a {
                    GR32  b => (mi.IMULrr32  a, b)
                    imm32 b => (mi.IMULrri32 a, b) }
                GR64 a {
                    imm32 b => (mi.IMULrr64i32 a, b) } }
            (ir.Load a) {
                (ir.FIAddr b) a {
                    mem32 b => (mi.MOVrm32 %rbp, b, none, none)
                    mem64 b => (mi.MOVrm64 %rbp, b, none, none) }
                GR64  a => (mi.MOVrm32 a, none, none, none)
            }
            (ir.Store a, b) {
                GR64   a {
                    imm32 b => (mi.MOVmi32 a, none, none, none, b)
                    GR32  b => (mi.MOVmr32 a, none, none, none, b)
                    GR64  b => (mi.MOVmr64 a, none, none, none, b) }
                (ir.FIAddr c) a {
                    mem32 c {
                        GR32  b => (mi.MOVmr32 %rbp, c, none, none, b)
                        imm32 b => (mi.MOVmi32 %rbp, c, none, none, b) } }
            }
            (ir.FIAddr a) {
                mem a => (mi.LEAr64m %rbp, a, none, none)
            }
            (ir.CopyFromReg a) => (mi.Copy a)
        );

        self.selected.insert(node, selected);

        if let Some(next) = node.next {
            selected.next = Some(self.run_on_node(tys, heap, next));
        }

        selected
    }
}
