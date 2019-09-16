// TODO: dirty code

use super::super::machine::{basic_block::*, frame_object::*, function::*, instr::*, module::*};
use super::super::register::*;
use super::{basic_block::*, function::*, module::*, node::*};
use crate::ir::types::*;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

pub struct ConvertToMachine {
    pub dag_bb_to_machine_bb: FxHashMap<DAGBasicBlockId, MachineBasicBlockId>,
}

impl ConvertToMachine {
    pub fn new() -> Self {
        Self {
            dag_bb_to_machine_bb: FxHashMap::default(),
        }
    }

    pub fn convert_module(&mut self, module: DAGModule) -> MachineModule {
        let mut machine_module = MachineModule::new(module.name.as_str());
        for (_, func) in module.functions {
            machine_module.add_function(self.convert_function(func));
        }
        machine_module
    }

    pub fn convert_function(&mut self, dag_func: DAGFunction) -> MachineFunction {
        self.dag_bb_to_machine_bb.clear();

        let mut machine_bb_arena: Arena<MachineBasicBlock> = Arena::new();
        let mut machine_bb_list: Vec<MachineBasicBlockId> = vec![];

        for dag_bb_id in &dag_func.dag_basic_blocks {
            let machine_bb_id = machine_bb_arena.alloc(MachineBasicBlock::new());
            self.dag_bb_to_machine_bb.insert(*dag_bb_id, machine_bb_id);
            machine_bb_list.push(machine_bb_id);
        }

        for (dag_bb, machine_bb) in &self.dag_bb_to_machine_bb {
            machine_bb_arena[*machine_bb].pred = dag_func.dag_basic_block_arena[*dag_bb]
                .pred
                .iter()
                .map(|bb| self.get_machine_bb(*bb))
                .collect();
            machine_bb_arena[*machine_bb].succ = dag_func.dag_basic_block_arena[*dag_bb]
                .succ
                .iter()
                .map(|bb| self.get_machine_bb(*bb))
                .collect();
        }

        let mut machine_instr_arena = Arena::new();

        for dag_bb_id in &dag_func.dag_basic_blocks {
            let node = &dag_func.dag_basic_block_arena[*dag_bb_id];
            let mut iseq = vec![];
            self.convert_dag(
                &dag_func,
                &mut machine_instr_arena,
                &mut FxHashMap::default(),
                &mut iseq,
                node.entry.unwrap(),
            );

            machine_bb_arena[self.get_machine_bb(*dag_bb_id)].iseq = Rc::new(RefCell::new(iseq));
        }

        MachineFunction::new(
            dag_func,
            machine_bb_arena,
            machine_bb_list,
            machine_instr_arena,
        )
    }

    pub fn convert_dag(
        &mut self,
        cur_func: &DAGFunction,
        machine_instr_arena: &mut Arena<MachineInstr>,
        dag_to_machine_reg: &mut FxHashMap<DAGNodeId, Option<MachineRegister>>,
        iseq: &mut Vec<MachineInstrId>,
        node_id: DAGNodeId,
    ) -> Option<MachineRegister> {
        if let Some(machine_register) = dag_to_machine_reg.get(&node_id) {
            return machine_register.clone();
        }

        fn iseq_push(
            machine_instr_arena: &mut Arena<MachineInstr>,
            iseq: &mut Vec<MachineInstrId>,
            instr: MachineInstr,
        ) -> MachineInstrId {
            let instr_id = machine_instr_arena.alloc(instr);
            iseq.push(instr_id);
            instr_id
        };

        #[rustfmt::skip]
        macro_rules! usual_oprand {($e:expr) => {
            self.usual_oprand(cur_func, machine_instr_arena, dag_to_machine_reg, iseq, $e)
        };}
        #[rustfmt::skip]
        macro_rules! bb {($id:expr)=>{cur_func.dag_arena[$id].as_basic_block()};}
        #[rustfmt::skip]
        macro_rules! cond_kind {($id:expr)=>{cur_func.dag_arena[$id].as_cond_kind()};}

        let node = &cur_func.dag_arena[node_id];

        let machine_instr_id = match node.kind {
            DAGNodeKind::Entry => None,
            DAGNodeKind::CopyToReg => {
                let val = usual_oprand!(node.operand[1]);
                let dst = match &cur_func.dag_arena[node.operand[0]].kind {
                    DAGNodeKind::Register(r) => MachineRegister::new(r.clone()),
                    _ => unreachable!(),
                };

                let dst_ty = dst.info_ref().ty.clone();
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new_with_def_reg(
                        MachineOpcode::Copy,
                        vec![val],
                        dst_ty,
                        vec![dst],
                    ),
                ))
            }
            DAGNodeKind::CopyFromReg => {
                let reg = usual_oprand!(node.operand[0]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::Copy,
                        vec![reg],
                        node.ty.clone(),
                    ),
                ))
            }
            DAGNodeKind::LoadRegOff => {
                let fi = usual_oprand!(node.operand[0]);
                let off = usual_oprand!(node.operand[1]);
                let align = usual_oprand!(node.operand[2]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::LoadRegOff,
                        vec![fi, off, align],
                        node.ty.clone(),
                    ),
                ))
            }
            DAGNodeKind::LoadFiOff => {
                let fi = usual_oprand!(node.operand[0]);
                let off = usual_oprand!(node.operand[1]);
                let align = usual_oprand!(node.operand[2]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::LoadFiOff,
                        vec![fi, off, align],
                        node.ty.clone(),
                    ),
                ))
            }
            DAGNodeKind::LoadFiConstOff => {
                let fi = usual_oprand!(node.operand[0]);
                let off = usual_oprand!(node.operand[1]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::LoadFiConstOff,
                        vec![fi, off],
                        node.ty.clone(),
                    ),
                ))
            }
            DAGNodeKind::StoreRegOff => {
                let fi = usual_oprand!(node.operand[0]);
                let off = usual_oprand!(node.operand[1]);
                let align = usual_oprand!(node.operand[2]);
                let new_src = usual_oprand!(node.operand[3]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::StoreRegOff,
                        vec![fi, off, align, new_src],
                        Type::Void,
                    ),
                ))
            }
            DAGNodeKind::StoreFiOff => {
                let fi = usual_oprand!(node.operand[0]);
                let off = usual_oprand!(node.operand[1]);
                let align = usual_oprand!(node.operand[2]);
                let new_src = usual_oprand!(node.operand[3]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::StoreFiOff,
                        vec![fi, off, align, new_src],
                        Type::Void,
                    ),
                ))
            }
            DAGNodeKind::StoreFiConstOff => {
                let fi = usual_oprand!(node.operand[0]);
                let off = usual_oprand!(node.operand[1]);
                let new_src = usual_oprand!(node.operand[2]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::StoreFiConstOff,
                        vec![fi, off, new_src],
                        Type::Void,
                    ),
                ))
            }
            DAGNodeKind::Load => {
                let new_op1 = usual_oprand!(node.operand[0]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::Load,
                        vec![new_op1],
                        node.ty.clone(),
                    ),
                ))
            }
            DAGNodeKind::Store => {
                let new_dst = usual_oprand!(node.operand[0]);
                let new_src = usual_oprand!(node.operand[1]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::Store,
                        vec![new_dst, new_src],
                        Type::Void,
                    ),
                ))
            }
            DAGNodeKind::Call => {
                let operands = node.operand.iter().map(|a| usual_oprand!(*a)).collect();
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::Call,
                        operands,
                        node.ty.clone(),
                    ),
                ))
            }
            DAGNodeKind::Phi => {
                let mut operands = vec![];
                let mut i = 0;
                while i < node.operand.len() {
                    operands.push(usual_oprand!(node.operand[i]));
                    operands.push(MachineOperand::Branch(
                        self.get_machine_bb(bb!(node.operand[i + 1])),
                    ));
                    i += 2;
                }
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::Phi,
                        operands,
                        node.ty.clone(),
                    ),
                ))
            }
            DAGNodeKind::Rem => {
                let eax =
                    RegisterInfo::new_phy_reg(Type::Int32, PhysReg(0)).into_machine_register();
                let edx =
                    RegisterInfo::new_phy_reg(Type::Int32, PhysReg(2)).into_machine_register();

                let op1 = usual_oprand!(node.operand[0]);
                let op2 = usual_oprand!(node.operand[1]);

                iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new_with_def_reg(
                        MachineOpcode::MOV32rX,
                        vec![op1],
                        Type::Int32, // TODO: support other types
                        vec![eax.clone()],
                    ),
                );

                iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new_with_imp_def_use(
                        MachineOpcode::CDQ,
                        vec![],
                        Type::Void,
                        vec![edx.clone()], //def
                        vec![eax.clone()], //use
                    ),
                );

                assert_eq!(op2.get_type(), Some(Type::Int32));
                let instr1 = MachineInstr::new(
                    &cur_func.vreg_gen,
                    MachineOpcode::Copy,
                    vec![op2],
                    Type::Int32, // TODO: support other types
                );
                let op2 = MachineOperand::Register(instr1.def[0].clone());
                iseq_push(machine_instr_arena, iseq, instr1);

                iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new_with_imp_def_use(
                        MachineOpcode::IDIV,
                        vec![op2],
                        Type::Void,
                        vec![eax.clone(), edx.clone()],
                        vec![eax, edx.clone()],
                    ),
                );

                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::Copy,
                        vec![MachineOperand::Register(edx)],
                        Type::Int32, // TODO
                    ),
                ))
            }
            DAGNodeKind::Add | DAGNodeKind::Sub | DAGNodeKind::Mul => {
                let op1 = usual_oprand!(node.operand[0]);
                let op2 = usual_oprand!(node.operand[1]);
                let op1_reg = op1.as_register().clone();
                let instr = MachineInstr::new(
                    &cur_func.vreg_gen,
                    match node.kind {
                        DAGNodeKind::Add => MachineOpcode::Add,
                        DAGNodeKind::Sub => MachineOpcode::Sub,
                        DAGNodeKind::Mul => MachineOpcode::Mul,
                        DAGNodeKind::Rem => MachineOpcode::Rem,
                        _ => unreachable!(),
                    },
                    vec![op1, op2],
                    node.ty.clone(),
                );
                let instr_tied = match node.kind {
                    DAGNodeKind::Add | DAGNodeKind::Sub => instr.set_tie_with_def(op1_reg),
                    _ => instr,
                };
                Some(iseq_push(machine_instr_arena, iseq, instr_tied))
            }
            DAGNodeKind::Setcc => {
                let new_op1 = usual_oprand!(node.operand[1]);
                let new_op2 = usual_oprand!(node.operand[2]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        match cond_kind!(node.operand[0]) {
                            CondKind::Eq => MachineOpcode::Seteq,
                            CondKind::Le => MachineOpcode::Setle,
                            CondKind::Lt => MachineOpcode::Setlt,
                        },
                        vec![new_op1, new_op2],
                        node.ty.clone(),
                    ),
                ))
            }
            DAGNodeKind::Br => Some(iseq_push(
                machine_instr_arena,
                iseq,
                MachineInstr::new(
                    &cur_func.vreg_gen,
                    MachineOpcode::Br,
                    vec![MachineOperand::Branch(
                        self.get_machine_bb(bb!(node.operand[0])),
                    )],
                    Type::Void,
                ),
            )),
            DAGNodeKind::BrCond => {
                let new_cond = usual_oprand!(node.operand[0]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::BrCond,
                        vec![
                            new_cond,
                            MachineOperand::Branch(self.get_machine_bb(bb!(node.operand[1]))),
                        ],
                        Type::Void,
                    ),
                ))
            }
            DAGNodeKind::Brcc => {
                let new_op0 = usual_oprand!(node.operand[1]);
                let new_op1 = usual_oprand!(node.operand[2]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        match cond_kind!(node.operand[0]) {
                            CondKind::Eq => MachineOpcode::BrccEq,
                            CondKind::Le => MachineOpcode::BrccLe,
                            CondKind::Lt => MachineOpcode::BrccLt,
                        },
                        vec![
                            new_op0,
                            new_op1,
                            MachineOperand::Branch(self.get_machine_bb(bb!(node.operand[3]))),
                        ],
                        Type::Void,
                    ),
                ))
            }
            DAGNodeKind::Ret => {
                let new_op1 = usual_oprand!(node.operand[0]);
                Some(iseq_push(
                    machine_instr_arena,
                    iseq,
                    MachineInstr::new(
                        &cur_func.vreg_gen,
                        MachineOpcode::Ret,
                        vec![new_op1],
                        Type::Void,
                    ),
                ))
            }
            _ => None,
        };

        let machine_register = match machine_instr_id {
            Some(id) => {
                if machine_instr_arena[id].def.len() > 0 {
                    Some(machine_instr_arena[id].def[0].clone())
                } else {
                    None
                }
            }
            None => None,
        };
        dag_to_machine_reg.insert(node_id, machine_register.clone());

        some_then!(next, node.next, {
            self.convert_dag(
                cur_func,
                machine_instr_arena,
                dag_to_machine_reg,
                iseq,
                next,
            );
        });

        machine_register
    }

    fn usual_oprand(
        &mut self,
        cur_func: &DAGFunction,
        machine_instr_arena: &mut Arena<MachineInstr>,
        dag_to_machine_reg: &mut FxHashMap<DAGNodeId, Option<MachineRegister>>,
        iseq: &mut Vec<MachineInstrId>,
        id: DAGNodeId,
    ) -> MachineOperand {
        let node = &cur_func.dag_arena[id];
        match node.kind {
            DAGNodeKind::Constant(c) => match c {
                ConstantKind::Int32(i) => MachineOperand::Constant(MachineConstant::Int32(i)),
            },
            DAGNodeKind::FrameIndex(idx, ref ty) => {
                MachineOperand::FrameIndex({ FrameIndexInfo::new(ty.clone(), idx.clone()) })
            }
            DAGNodeKind::GlobalAddress(ref g) => match g {
                GlobalValueKind::FunctionName(n) => {
                    MachineOperand::GlobalAddress(GlobalValueInfo::FunctionName(n.clone()))
                }
            },
            DAGNodeKind::None => MachineOperand::None,
            DAGNodeKind::BasicBlock(_) => unimplemented!(),
            DAGNodeKind::Register(ref r) => {
                MachineOperand::Register(MachineRegister::new(r.clone()))
            }
            _ => MachineOperand::Register(
                self.convert_dag(cur_func, machine_instr_arena, dag_to_machine_reg, iseq, id)
                    .unwrap(),
            ),
        }
    }

    fn get_machine_bb(&self, dag_bb_id: DAGBasicBlockId) -> MachineBasicBlockId {
        *self.dag_bb_to_machine_bb.get(&dag_bb_id).unwrap()
    }
}
