// TODO: dirty code

use super::super::machine::{basic_block::*, function::*, instr::*, module::*};
use super::{basic_block::*, function::*, module::*, node::*};
use id_arena::*;
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

pub struct ConvertToMachine<'a> {
    pub module: &'a DAGModule,
    pub dag_node_id_to_machine_register: FxHashMap<DAGNodeId, Option<MachineRegister>>,
    pub dag_bb_to_machine_bb: FxHashMap<DAGBasicBlockId, MachineBasicBlockId>,
}

impl<'a> ConvertToMachine<'a> {
    pub fn new(module: &'a DAGModule) -> Self {
        Self {
            module,
            dag_node_id_to_machine_register: FxHashMap::default(),
            dag_bb_to_machine_bb: FxHashMap::default(),
        }
    }

    pub fn convert_module(&mut self) -> MachineModule {
        let mut machine_module = MachineModule::new(self.module.name.as_str());
        for (_, func) in &self.module.functions {
            machine_module.add_function(self.convert_function(func));
        }
        machine_module
    }

    pub fn convert_function(&mut self, dag_func: &DAGFunction) -> MachineFunction {
        self.dag_node_id_to_machine_register.clear();
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

            machine_bb_arena[self.get_machine_bb(dag_bb_id)].iseq = Rc::new(RefCell::new(iseq));
        }

        MachineFunction::new(dag_func, machine_bb_arena, machine_instr_arena)
    }

    pub fn convert_dag(
        &mut self,
        cur_func: &DAGFunction,
        machine_instr_arena: &mut Arena<MachineInstr>,
        iseq: &mut Vec<MachineInstrId>,
        node_id: DAGNodeId,
    ) -> Option<MachineRegister> {
        if let Some(machine_register) = self.dag_node_id_to_machine_register.get(&node_id) {
            return machine_register.clone();
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
                )))
            }
            DAGNodeKind::Store(dstid, srcid) => {
                let new_dst = usual_oprand!(dstid);
                let new_src = usual_oprand!(srcid);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Store,
                    vec![new_dst, new_src],
                    None,
                )))
            }
            DAGNodeKind::Call(f, ref args) => {
                let mut operands = vec![usual_oprand!(f)];
                for arg in args {
                    operands.push(usual_oprand!(*arg));
                }
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Call,
                    operands,
                    node.ty.clone(),
                )))
            }
            DAGNodeKind::Phi(ref pairs) => {
                let mut operands = vec![];
                for (val, bb) in pairs {
                    operands.push(usual_oprand!(*val));
                    operands.push(MachineOprand::Branch(self.get_machine_bb(*bb)));
                }
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Phi,
                    operands,
                    node.ty.clone(),
                )))
            }
            DAGNodeKind::Add(op1, op2) => {
                let new_op1 = usual_oprand!(op1);
                let new_op2 = usual_oprand!(op2);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Add,
                    vec![new_op1, new_op2],
                    node.ty.clone(),
                )))
            }
            DAGNodeKind::Sub(op1, op2) => {
                let new_op1 = usual_oprand!(op1);
                let new_op2 = usual_oprand!(op2);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Sub,
                    vec![new_op1, new_op2],
                    node.ty.clone(),
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
                )))
            }
            DAGNodeKind::Br(dag_bb_id) => Some(machine_instr_arena.alloc(MachineInstr::new(
                MachineOpcode::Br,
                vec![MachineOprand::Branch(self.get_machine_bb(dag_bb_id))],
                None,
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
                )))
            }
            DAGNodeKind::Brcc(cond, op0, op1, dag_bb_id) => {
                let new_op0 = usual_oprand!(op0);
                let new_op1 = usual_oprand!(op1);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    match cond {
                        CondKind::Eq => MachineOpcode::BrccEq,
                        CondKind::Le => MachineOpcode::BrccLe,
                    },
                    vec![
                        new_op0,
                        new_op1,
                        MachineOprand::Branch(self.get_machine_bb(dag_bb_id)),
                    ],
                    None,
                )))
            }
            DAGNodeKind::Ret(op1) => {
                let new_op1 = usual_oprand!(op1);
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Ret,
                    vec![new_op1],
                    None,
                )))
            }
            DAGNodeKind::GlobalAddress(_)
            | DAGNodeKind::Constant(_)
            | DAGNodeKind::FrameIndex(_, _) => None,
        };

        some_then!(id, machine_instr_id, { iseq.push(id) });
        let machine_register = match machine_instr_id {
            Some(id) => Some(MachineRegister::new(machine_instr_arena[id].reg.clone())),
            None => None,
        };
        self.dag_node_id_to_machine_register
            .insert(node_id, machine_register.clone());

        some_then!(next, node.next, {
            self.convert_dag(cur_func, machine_instr_arena, iseq, next);
        });

        machine_register
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
            DAGNodeKind::GlobalAddress(ref g) => MachineOprand::GlobalAddress(match g {
                GlobalValueKind::FunctionName(n) => GlobalValueInfo::FunctionName(n.clone()),
            }),
            _ => MachineOprand::Register(
                self.convert_dag(cur_func, machine_instr_arena, iseq, node_id)
                    .unwrap(),
            ),
        }
    }

    fn get_machine_bb(&self, dag_bb_id: DAGBasicBlockId) -> MachineBasicBlockId {
        *self.dag_bb_to_machine_bb.get(&dag_bb_id).unwrap()
    }
}
