// TODO: dirty code

use super::super::machine::{basic_block::*, function::*, instr::*};
use super::{basic_block::*, convert::*, node::*};
use crate::ir::module::*;
use id_arena::*;
use rustc_hash::{FxHashMap, FxHashSet};

pub struct ConvertToMachine<'a> {
    pub module: &'a Module,
    pub vreg_count: usize,
    pub dag_node_id_to_machine_instr_id: FxHashMap<DAGNodeId, Option<MachineInstrId>>,
    pub dag_bb_to_machine_bb: FxHashMap<DAGBasicBlockId, MachineBasicBlockId>,
}

impl<'a> ConvertToMachine<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            vreg_count: 0,
            dag_node_id_to_machine_instr_id: FxHashMap::default(),
            dag_bb_to_machine_bb: FxHashMap::default(),
        }
    }

    pub fn next_vreg(&mut self) -> usize {
        self.vreg_count += 1;
        self.vreg_count
    }

    pub fn convert_function(&mut self, dag_func: &DAGFunction) -> MachineFunction {
        self.dag_node_id_to_machine_instr_id.clear();
        self.dag_bb_to_machine_bb.clear();

        let mut machine_bb_arena: Arena<MachineBasicBlock> = Arena::new();

        for (dag_bb_id, _) in &dag_func.dag_basic_blocks {
            self.dag_bb_to_machine_bb
                .insert(dag_bb_id, machine_bb_arena.alloc(MachineBasicBlock::new()));
        }

        for (dag_bb, machine_bb) in &self.dag_bb_to_machine_bb {
            machine_bb_arena[*machine_bb].pred = dag_func.dag_basic_blocks[*dag_bb]
                .pred
                .iter()
                .map(|bb| self.get_machine_bb(*bb))
                .collect();
            machine_bb_arena[*machine_bb].succ = dag_func.dag_basic_blocks[*dag_bb]
                .succ
                .iter()
                .map(|bb| self.get_machine_bb(*bb))
                .collect();
        }

        let mut machine_instr_arena = Arena::new();

        for (dag_bb_id, node) in &dag_func.dag_basic_blocks {
            let mut iseq = vec![];
            self.convert_dag(
                &dag_func,
                &mut machine_instr_arena,
                &mut iseq,
                node.entry.unwrap(),
            );

            when_debug!({
                println!("dag bb id({})", dag_bb_id.index());
                for instr in &iseq {
                    println!("{}: {:?}", instr.index(), machine_instr_arena[*instr]);
                }
            });

            machine_bb_arena[self.get_machine_bb(dag_bb_id)].iseq = iseq;
        }

        MachineFunction::new(self.module.function_ref(dag_func.func_id), machine_bb_arena)
    }

    pub fn convert_dag(
        &mut self,
        cur_func: &DAGFunction,
        machine_instr_arena: &mut Arena<MachineInstr>,
        iseq: &mut Vec<MachineInstrId>,
        node_id: DAGNodeId,
    ) -> Option<MachineInstrId> {
        if let Some(machine_instr_id) = self.dag_node_id_to_machine_instr_id.get(&node_id) {
            return *machine_instr_id;
        }

        macro_rules! usual_oprand {
            ($e:expr) => {
                self.usual_oprand(cur_func, machine_instr_arena, iseq, $e)
            };
        }

        let node = &cur_func.dag_arena[node_id];

        let machine_instr_id = match node.kind {
            DAGNodeKind::Entry => None,
            DAGNodeKind::Load(op1) => {
                let new_op1 = usual_oprand!(op1);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Load,
                    vec![new_op1],
                    node.ty.clone(),
                    self.next_vreg(),
                )))
            }
            DAGNodeKind::Store(dstid, srcid) => {
                let dst = &cur_func.dag_arena[dstid];
                let new_dst = match dst.kind {
                    DAGNodeKind::FrameIndex(i, ref ty) => {
                        MachineOprand::FrameIndex(FrameIndexInfo::new(ty.clone(), i))
                    }
                    _ => unimplemented!(),
                };
                let new_src = usual_oprand!(srcid);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Store,
                    vec![new_dst, new_src],
                    None,
                    0,
                )))
            }
            DAGNodeKind::Add(op1, op2) => {
                let new_op1 = usual_oprand!(op1);
                let new_op2 = usual_oprand!(op2);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Add,
                    vec![new_op1, new_op2],
                    node.ty.clone(),
                    self.next_vreg(),
                )))
            }
            DAGNodeKind::Setcc(kind, op1, op2) => {
                let new_op1 = usual_oprand!(op1);
                let new_op2 = usual_oprand!(op2);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    match kind {
                        CondKind::Eq => MachineOpcode::Seteq,
                        CondKind::Le => MachineOpcode::Setle,
                    },
                    vec![new_op1, new_op2],
                    node.ty.clone(),
                    self.next_vreg(),
                )))
            }
            DAGNodeKind::Br(dag_bb_id) => Some(machine_instr_arena.alloc(MachineInstr::new(
                MachineOpcode::Br,
                vec![MachineOprand::Branch(self.get_machine_bb(dag_bb_id))],
                None,
                0,
            ))),
            DAGNodeKind::BrCond(cond, dag_bb_id) => {
                let new_cond = usual_oprand!(cond);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::BrCond,
                    vec![
                        new_cond,
                        MachineOprand::Branch(self.get_machine_bb(dag_bb_id)),
                    ],
                    None,
                    0,
                )))
            }
            DAGNodeKind::Ret(op1) => {
                let new_op1 = usual_oprand!(op1);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Ret,
                    vec![new_op1],
                    None,
                    0,
                )))
            }
            _ => None,
        };

        some_then!(id, machine_instr_id, { iseq.push(id) });
        self.dag_node_id_to_machine_instr_id
            .insert(node_id, machine_instr_id);

        some_then!(next, node.next, {
            self.convert_dag(cur_func, machine_instr_arena, iseq, next);
        });

        machine_instr_id
    }

    fn usual_oprand(
        &mut self,
        cur_func: &DAGFunction,
        machine_instr_arena: &mut Arena<MachineInstr>,
        iseq: &mut Vec<MachineInstrId>,
        node_id: DAGNodeId,
    ) -> MachineOprand {
        let node = &cur_func.dag_arena[node_id];
        match node.kind {
            DAGNodeKind::Constant(ConstantKind::Int32(i)) => {
                MachineOprand::Constant(MachineConstant::Int32(i))
            }
            DAGNodeKind::FrameIndex(i, ref ty) => {
                MachineOprand::FrameIndex(FrameIndexInfo::new(ty.clone(), i))
            }
            _ => MachineOprand::Instr(
                self.convert_dag(cur_func, machine_instr_arena, iseq, node_id)
                    .unwrap(),
            ),
        }
    }

    fn get_machine_bb(&self, dag_bb_id: DAGBasicBlockId) -> MachineBasicBlockId {
        *self.dag_bb_to_machine_bb.get(&dag_bb_id).unwrap()
    }
}
