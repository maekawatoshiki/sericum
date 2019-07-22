// TODO: dirty code

use super::super::lower::instr::*;
use super::{convert::*, node::*};
use crate::ir::module::*;
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
        for (id, node) in &dag_func.dag_basic_blocks {
            let mut iseq = vec![];
            self.select_dag(&dag_func, &mut iseq, node.entry.unwrap());
            println!("iseq{}:\n{:?}", id.index(), iseq);
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
                let new_op1 = match op1.kind {
                    DAGNodeKind::FrameIndex(i, ref ty) => {
                        LowerOprand::FrameIndex(FrameIndexInfo::new(ty.clone(), i))
                    }
                    _ => {
                        self.select_dag(dag_func, iseq, op1id);
                        LowerOprand::VReg(VRegInfo::new(
                            op1.ty.clone().unwrap(),
                            *self.dag_id_to_vreg.get(&op1id).unwrap(),
                        ))
                    }
                };
                iseq.push(LowerInstr::new(
                    LowerOpcode::Load,
                    vec![new_op1],
                    node.ty.clone(),
                    vreg,
                ));
            }
            DAGNodeKind::Store(dstid, srcid) => {
                let dst = &dag_func.dag_arena[dstid];
                let _src = &dag_func.dag_arena[srcid];
                let new_dst = match dst.kind {
                    DAGNodeKind::FrameIndex(i, ref ty) => {
                        LowerOprand::FrameIndex(FrameIndexInfo::new(ty.clone(), i))
                    }
                    _ => unimplemented!(),
                };
                iseq.push(LowerInstr::new(LowerOpcode::Store, vec![new_dst], None, 0));
            }
            _ => {}
        }

        some_then!(next, node.next, self.select_dag(dag_func, iseq, next));
    }
}
