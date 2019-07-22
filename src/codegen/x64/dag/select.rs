use super::super::lower::instr::*;
use super::{convert::*, node::*};
use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;
use rustc_hash::FxHashMap;

pub struct SelectInstruction<'a> {
    pub module: &'a Module,
    pub dag_id_to_vreg: FxHashMap<DAGNodeId, usize>,
    pub vreg_count: usize,
}

impl<'a> SelectInstruction<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            dag_id_to_vreg: FxHashMap::default(),
            vreg_count: 0,
        }
    }

    pub fn next_vreg(&mut self) -> usize {
        self.vreg_count += 1;
        self.vreg_count
    }

    pub fn select_function(&mut self, dag_func: &DAGFunction) {
        for (_, node) in &dag_func.bb_to_node {
            let mut iseq = vec![];
            self.select_dag(&dag_func, &mut iseq, *node);
            println!("iseq:\n{:?}", iseq);
        }
    }

    pub fn select_dag(
        &mut self,
        dag_func: &DAGFunction,
        iseq: &mut Vec<LowerInstr>,
        node_id: DAGNodeId,
    ) {
        let node = &dag_func.dag_arena[node_id];

        match node.kind {
            DAGNodeKind::Entry => {}
            DAGNodeKind::Load(op1id) => {
                let op1 = &dag_func.dag_arena[op1id];
                let vreg = self.next_vreg();
                self.dag_id_to_vreg.insert(node_id, vreg);
                match op1.kind {
                    DAGNodeKind::FrameIndex(i) => {
                        iseq.push(LowerInstr::new(
                            LowerOpcode::Load,
                            vec![LowerOprand::FrameIndex(FrameIndexInfo::new(
                                node.ty.as_ref().unwrap().clone(),
                                i,
                            ))],
                            node.ty.as_ref().unwrap().clone(),
                            vreg,
                        ));
                    }
                    _ => self.select_dag(dag_func, iseq, op1id),
                }
            }
            DAGNodeKind::Store(dstid, srcid) => {
                let dst = &dag_func.dag_arena[dstid];
                let src = &dag_func.dag_arena[srcid];
                match dst.kind {
                    DAGNodeKind::FrameIndex(i) => {
                        iseq.push(LowerInstr::new(
                            LowerOpcode::Load,
                            vec![LowerOprand::FrameIndex(FrameIndexInfo::new(
                                node.ty.as_ref().unwrap().clone(),
                                i,
                            ))],
                            node.ty.as_ref().unwrap().clone(),
                            0,
                        ));
                    }
                    _ => self.select_dag(dag_func, iseq, op1id),
                }
            }
            _ => {}
        }

        some_then!(next, node.next, self.select_dag(dag_func, iseq, next));
    }
}
