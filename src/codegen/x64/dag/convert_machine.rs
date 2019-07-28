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
            DAGNodeKind::Load => {
                let new_op1 = usual_oprand!(node.operand[0].id());
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Load,
                    vec![new_op1],
                    node.ty.clone(),
                )))
            }
            DAGNodeKind::Store => {
                let new_dst = usual_oprand!(node.operand[0].id());
                let new_src = usual_oprand!(node.operand[1].id());
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Store,
                    vec![new_dst, new_src],
                    None,
                )))
            }
            DAGNodeKind::Call => {
                let mut operands = vec![usual_oprand!(node.operand[0].id())];
                for arg in &node.operand[1..] {
                    operands.push(usual_oprand!(arg.id()));
                }
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Call,
                    operands,
                    node.ty.clone(),
                )))
            }
            DAGNodeKind::Phi => {
                let mut operands = vec![];
                let mut i = 0;
                while i < node.operand.len() {
                    operands.push(usual_oprand!(node.operand[i].id()));
                    operands.push(MachineOperand::Branch(
                        self.get_machine_bb(node.operand[i + 1].basic_block()),
                    ));
                    i += 2;
                }
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Phi,
                    operands,
                    node.ty.clone(),
                )))
            }
            DAGNodeKind::Add | DAGNodeKind::Sub | DAGNodeKind::Mul | DAGNodeKind::Rem => {
                let new_op1 = usual_oprand!(node.operand[0].id());
                let new_op2 = usual_oprand!(node.operand[1].id());
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    match node.kind {
                        DAGNodeKind::Add => MachineOpcode::Add,
                        DAGNodeKind::Sub => MachineOpcode::Sub,
                        DAGNodeKind::Mul => MachineOpcode::Mul,
                        DAGNodeKind::Rem => MachineOpcode::Rem,
                        _ => unreachable!(),
                    },
                    vec![new_op1, new_op2],
                    node.ty.clone(),
                )))
            }
            DAGNodeKind::Setcc => {
                let new_op1 = usual_oprand!(node.operand[1].id());
                let new_op2 = usual_oprand!(node.operand[2].id());
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    match node.operand[0].cond_kind() {
                        CondKind::Eq => MachineOpcode::Seteq,
                        CondKind::Le => MachineOpcode::Setle,
                    },
                    vec![new_op1, new_op2],
                    node.ty.clone(),
                )))
            }
            DAGNodeKind::Br => Some(machine_instr_arena.alloc(MachineInstr::new(
                MachineOpcode::Br,
                vec![MachineOperand::Branch(
                    self.get_machine_bb(node.operand[0].basic_block()),
                )],
                None,
            ))),
            DAGNodeKind::BrCond => {
                let new_cond = usual_oprand!(node.operand[0].id());
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::BrCond,
                    vec![
                        new_cond,
                        MachineOperand::Branch(self.get_machine_bb(node.operand[1].basic_block())),
                    ],
                    None,
                )))
            }
            DAGNodeKind::Brcc => {
                let new_op0 = usual_oprand!(node.operand[1].id());
                let new_op1 = usual_oprand!(node.operand[2].id());
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    match node.operand[0].cond_kind() {
                        CondKind::Eq => MachineOpcode::BrccEq,
                        CondKind::Le => MachineOpcode::BrccLe,
                    },
                    vec![
                        new_op0,
                        new_op1,
                        MachineOperand::Branch(self.get_machine_bb(node.operand[3].basic_block())),
                    ],
                    None,
                )))
            }
            DAGNodeKind::Ret => {
                let new_op1 = usual_oprand!(node.operand[0].id());
                Some(machine_instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Ret,
                    vec![new_op1],
                    None,
                )))
            }
            DAGNodeKind::None
            | DAGNodeKind::GlobalAddress
            | DAGNodeKind::Constant
            | DAGNodeKind::FrameIndex => None,
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
    ) -> MachineOperand {
        let node = &cur_func.dag_arena[node_id];
        match node.kind {
            DAGNodeKind::Constant => match node.operand[0].constant() {
                ConstantKind::Int32(i) => MachineOperand::Constant(MachineConstant::Int32(i)),
            },
            DAGNodeKind::FrameIndex => MachineOperand::FrameIndex({
                let (idx, ty) = node.operand[0].frame_idx();
                FrameIndexInfo::new(ty.clone(), idx)
            }),
            DAGNodeKind::GlobalAddress => {
                MachineOperand::GlobalAddress(match node.operand[0].global_addr() {
                    GlobalValueKind::FunctionName(n) => GlobalValueInfo::FunctionName(n.clone()),
                })
            }
            DAGNodeKind::None => MachineOperand::None,
            _ => MachineOperand::Register(
                self.convert_dag(cur_func, machine_instr_arena, iseq, node_id)
                    .unwrap(),
            ),
        }
    }

    fn get_machine_bb(&self, dag_bb_id: DAGBasicBlockId) -> MachineBasicBlockId {
        *self.dag_bb_to_machine_bb.get(&dag_bb_id).unwrap()
    }
}
