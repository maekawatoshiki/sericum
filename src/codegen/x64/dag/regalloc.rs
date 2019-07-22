use super::{basic_block::*, convert::*, node::*};
// use super::{convert::*, node::*};
// use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use rustc_hash::FxHashMap;

pub struct PhysicalRegisterAllocator {}

impl PhysicalRegisterAllocator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_function(&mut self, dag_func: &DAGFunction) {
        self.collect_regs(dag_func);
        self.scan(dag_func);
    }

    fn scan(&mut self, cur_func: &DAGFunction) {
        let mut used = FxHashMap::default();
        for (_, bb) in &cur_func.dag_basic_blocks {
            self.scan_on_node(cur_func, &mut used, bb.entry.unwrap());
        }
    }

    fn scan_on_node(
        &mut self,
        cur_func: &DAGFunction,
        used: &mut FxHashMap<usize, DAGNodeId>,
        node_id: DAGNodeId,
    ) {
        let node = &cur_func.dag_arena[node_id];
        let num_reg = 4;

        if node.reg.borrow().last_use.is_none() {
            return;
        }

        // let mut found = false;
        // for i in 0..num_reg - 1 {
        //     if used.contains_key(&i) {
        //         let target_last_use_id = cur_func.dag_arena[*used.get(&i).unwrap()]
        //             .reg
        //             .borrow()
        //             .last_use
        //             .unwrap();
        //         let target_last_use = vreg!(f; target_last_use_id);
        //         if vreg!(instr) < target_last_use {
        //             continue;
        //         }
        //     }
        //
        //     node.set_phy_reg(i, false);
        //     used.insert(i, node_id);
        //     found = true;
        //     break;
        // }
        // if found {
        //     continue;
        // }
    }

    fn collect_regs(&mut self, cur_func: &DAGFunction) {
        for (_, bb) in &cur_func.dag_basic_blocks {
            let last_node_id = self.collect_regs_on_node(cur_func, bb.entry.unwrap());

            for out in &bb.liveness.borrow().live_out {
                cur_func.dag_arena[*out].set_last_use(Some(last_node_id));
            }
        }
    }

    fn collect_regs_on_node(&mut self, cur_func: &DAGFunction, node_id: DAGNodeId) -> DAGNodeId {
        let node = &cur_func.dag_arena[node_id];

        match node.kind {
            DAGNodeKind::Store(op1, op2)
            | DAGNodeKind::Add(op1, op2)
            | DAGNodeKind::Setcc(_, op1, op2) => {
                cur_func.dag_arena[op1].set_last_use(Some(node_id));
                cur_func.dag_arena[op2].set_last_use(Some(node_id));
            }
            DAGNodeKind::Load(op1) | DAGNodeKind::BrCond(op1, _) | DAGNodeKind::Ret(op1) => {
                cur_func.dag_arena[op1].set_last_use(Some(node_id));
            }
            DAGNodeKind::Entry
            | DAGNodeKind::FrameIndex(_, _)
            | DAGNodeKind::Br(_)
            | DAGNodeKind::Constant(_) => {}
        }

        match node.next {
            Some(next) => self.collect_regs_on_node(cur_func, next),
            None => node_id,
        }
    }
}
