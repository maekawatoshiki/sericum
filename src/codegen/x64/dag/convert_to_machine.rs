// TODO: dirty code

use super::super::machine::{basic_block::*, frame_object::*, function::*, instr::*, module::*};
use super::super::register::*;
use super::{basic_block::*, function::*, module::*, node::*};
use crate::ir::types::*;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

pub struct ConvertToMachine {
    pub dag_bb_to_machine_bb: FxHashMap<DAGBasicBlockId, MachineBasicBlockId>,
    pub node_id_to_machine_instr_id: FxHashMap<DAGNodeId, MachineInstrId>,
}

pub struct ConversionInfo<'a> {
    cur_func: &'a DAGFunction,
    machine_instr_arena: &'a mut InstructionArena,
    dag_to_reg: FxHashMap<DAGNodeId, Option<MachineRegister>>,
    cur_bb: MachineBasicBlockId,
    iseq: &'a mut Vec<MachineInstrId>,
}

impl ConvertToMachine {
    pub fn new() -> Self {
        Self {
            dag_bb_to_machine_bb: FxHashMap::default(),
            node_id_to_machine_instr_id: FxHashMap::default(),
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

        let mut machine_instr_arena = InstructionArena::new();

        for dag_bb_id in &dag_func.dag_basic_blocks {
            let node = &dag_func.dag_basic_block_arena[*dag_bb_id];
            let bb_id = self.get_machine_bb(*dag_bb_id);
            let mut iseq = vec![];

            self.convert_dag(
                &mut ConversionInfo {
                    cur_func: &dag_func,
                    machine_instr_arena: &mut machine_instr_arena,
                    dag_to_reg: FxHashMap::default(),
                    cur_bb: bb_id,
                    iseq: &mut iseq,
                },
                node.entry.unwrap(),
            );

            machine_bb_arena[bb_id].iseq = RefCell::new(iseq);
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
        conv_info: &mut ConversionInfo,
        node_id: DAGNodeId,
    ) -> Option<MachineRegister> {
        if let Some(machine_register) = conv_info.dag_to_reg.get(&node_id) {
            return machine_register.clone();
        }

        let machine_register = match self.convert_dag_to_machine_instr(conv_info, node_id) {
            Some(id) => {
                let instr = &conv_info.machine_instr_arena[id];
                instr.get_def_reg().map(|r| r.clone())
            }
            None => None,
        };
        conv_info
            .dag_to_reg
            .insert(node_id, machine_register.clone());

        let node = &conv_info.cur_func.dag_arena[node_id];
        some_then!(next, node.next, {
            self.convert_dag(conv_info, next);
        });

        machine_register
    }

    fn convert_dag_to_machine_instr(
        &mut self,
        conv_info: &mut ConversionInfo,
        node_id: DAGNodeId,
    ) -> Option<MachineInstrId> {
        if let Some(machine_instr_id) = self.node_id_to_machine_instr_id.get(&node_id) {
            return Some(*machine_instr_id);
        }

        #[rustfmt::skip]
        macro_rules! bb {($id:expr)=>{conv_info.cur_func.dag_arena[$id].as_basic_block()};}
        #[rustfmt::skip]
        macro_rules! cond_kind {($id:expr)=>{conv_info.cur_func.dag_arena[$id].as_cond_kind()};}

        let node = &conv_info.cur_func.dag_arena[node_id];

        let machine_instr_id = match node.kind {
            DAGNodeKind::Entry => None,
            DAGNodeKind::CopyToReg => {
                let val = self.usual_operand(conv_info, node.operand[1]);
                let dst = match &conv_info.cur_func.dag_arena[node.operand[0]].kind {
                    DAGNodeKind::Register(r) => MachineRegister::new(r.clone()),
                    _ => unreachable!(),
                };

                Some(conv_info.push_instr(MachineInstr::new_with_def_reg(
                    MachineOpcode::Copy,
                    vec![val],
                    vec![dst],
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::CopyFromReg => {
                let reg = self.usual_operand(conv_info, node.operand[0]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::Copy,
                    vec![reg],
                    &node.ty,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::LoadRegOff => {
                let base = self.usual_operand(conv_info, node.operand[0]);
                let off = self.usual_operand(conv_info, node.operand[1]);
                let align = self.usual_operand(conv_info, node.operand[2]);
                // MachineOperand::Mem(base, off, align);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::LoadRegOff,
                    vec![base, off, align],
                    &node.ty,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::LoadFiOff => {
                let fi = self.usual_operand(conv_info, node.operand[0]);
                let off = self.usual_operand(conv_info, node.operand[1]);
                let align = self.usual_operand(conv_info, node.operand[2]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::LoadFiOff,
                    vec![fi, off, align],
                    &node.ty,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::LoadFiConstOff => {
                let fi = self.usual_operand(conv_info, node.operand[0]);
                let off = self.usual_operand(conv_info, node.operand[1]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::LoadFiConstOff,
                    vec![fi, off],
                    &node.ty,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::StoreRegOff => {
                let base = self.usual_operand(conv_info, node.operand[0]);
                let off = self.usual_operand(conv_info, node.operand[1]);
                let align = self.usual_operand(conv_info, node.operand[2]);
                let new_src = self.usual_operand(conv_info, node.operand[3]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::StoreRegOff,
                    vec![base, off, align, new_src],
                    &Type::Void,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::StoreFiOff => {
                let fi = self.usual_operand(conv_info, node.operand[0]);
                let off = self.usual_operand(conv_info, node.operand[1]);
                let align = self.usual_operand(conv_info, node.operand[2]);
                let new_src = self.usual_operand(conv_info, node.operand[3]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::StoreFiOff,
                    vec![fi, off, align, new_src],
                    &Type::Void,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::StoreFiConstOff => {
                let fi = self.usual_operand(conv_info, node.operand[0]);
                let off = self.usual_operand(conv_info, node.operand[1]);
                let new_src = self.usual_operand(conv_info, node.operand[2]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::StoreFiConstOff,
                    vec![fi, off, new_src],
                    &Type::Void,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::Load => {
                let new_op1 = self.usual_operand(conv_info, node.operand[0]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::Load,
                    vec![new_op1],
                    &node.ty,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::Store => {
                let new_dst = self.usual_operand(conv_info, node.operand[0]);
                let new_src = self.usual_operand(conv_info, node.operand[1]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::Store,
                    vec![new_dst, new_src],
                    &Type::Void,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::Call => Some(self.convert_call_dag(conv_info, node)),
            DAGNodeKind::Phi => {
                let mut operands = vec![];
                let mut i = 0;
                while i < node.operand.len() {
                    operands.push(self.usual_operand(conv_info, node.operand[i]));
                    operands.push(MachineOperand::Branch(
                        self.get_machine_bb(bb!(node.operand[i + 1])),
                    ));
                    i += 2;
                }
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::Phi,
                    operands,
                    &node.ty,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::Rem => {
                let eax = RegisterInfo::new_phy_reg(Type::Int32, PhysReg(0))
                    .with_vreg(conv_info.cur_func.vreg_gen.next_vreg())
                    .into_machine_register();
                let edx = RegisterInfo::new_phy_reg(Type::Int32, PhysReg(2))
                    .with_vreg(conv_info.cur_func.vreg_gen.next_vreg())
                    .into_machine_register();

                let op1 = self.usual_operand(conv_info, node.operand[0]);
                let op2 = self.usual_operand(conv_info, node.operand[1]);

                conv_info.push_instr(
                    MachineInstr::new_simple(
                        mov_n_rx(32, &op1).unwrap(),
                        vec![op1],
                        conv_info.cur_bb,
                    )
                    .with_def(vec![eax.clone()]),
                );

                conv_info.push_instr(
                    MachineInstr::new_simple(MachineOpcode::CDQ, vec![], conv_info.cur_bb)
                        .with_imp_def(edx.clone())
                        .with_imp_use(eax.clone()),
                );

                assert_eq!(op2.get_type(), Some(Type::Int32));
                let instr1 = MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    mov_n_rx(32, &op2).unwrap(),
                    vec![op2],
                    &Type::Int32, // TODO: support other types
                    conv_info.cur_bb,
                );
                let op2 = MachineOperand::Register(instr1.def[0].clone());
                conv_info.push_instr(instr1);

                conv_info.push_instr(
                    MachineInstr::new_simple(MachineOpcode::IDIV, vec![op2], conv_info.cur_bb)
                        .with_imp_defs(vec![eax.clone(), edx.clone()])
                        .with_imp_uses(vec![eax, edx.clone()]),
                );

                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::Copy,
                    vec![MachineOperand::Register(edx)],
                    &Type::Int32, // TODO
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::Add | DAGNodeKind::Sub | DAGNodeKind::Mul => {
                let op1 = self.usual_operand(conv_info, node.operand[0]);
                let op2 = self.usual_operand(conv_info, node.operand[1]);
                let op1_reg = op1.as_register().clone();
                let instr = MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    match node.kind {
                        DAGNodeKind::Add => MachineOpcode::Add,
                        DAGNodeKind::Sub => MachineOpcode::Sub,
                        DAGNodeKind::Mul => MachineOpcode::Mul,
                        DAGNodeKind::Rem => MachineOpcode::Rem,
                        _ => unreachable!(),
                    },
                    vec![op1, op2],
                    &node.ty,
                    conv_info.cur_bb,
                );
                let instr_tied = match node.kind {
                    DAGNodeKind::Add | DAGNodeKind::Sub => instr.set_tie_with_def(op1_reg),
                    _ => instr,
                };
                Some(conv_info.push_instr(instr_tied))
            }
            DAGNodeKind::Setcc => {
                let new_op1 = self.usual_operand(conv_info, node.operand[1]);
                let new_op2 = self.usual_operand(conv_info, node.operand[2]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    match cond_kind!(node.operand[0]) {
                        CondKind::Eq => MachineOpcode::Seteq,
                        CondKind::Le => MachineOpcode::Setle,
                        CondKind::Lt => MachineOpcode::Setlt,
                    },
                    vec![new_op1, new_op2],
                    &node.ty,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::Br => Some(conv_info.push_instr(MachineInstr::new(
                &conv_info.cur_func.vreg_gen,
                MachineOpcode::Br,
                vec![MachineOperand::Branch(
                    self.get_machine_bb(bb!(node.operand[0])),
                )],
                &Type::Void,
                conv_info.cur_bb,
            ))),
            DAGNodeKind::BrCond => {
                let new_cond = self.usual_operand(conv_info, node.operand[0]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::BrCond,
                    vec![
                        new_cond,
                        MachineOperand::Branch(self.get_machine_bb(bb!(node.operand[1]))),
                    ],
                    &Type::Void,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::Brcc => {
                let new_op0 = self.usual_operand(conv_info, node.operand[1]);
                let new_op1 = self.usual_operand(conv_info, node.operand[2]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
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
                    &Type::Void,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::Ret => {
                let new_op1 = self.usual_operand(conv_info, node.operand[0]);
                Some(conv_info.push_instr(MachineInstr::new(
                    &conv_info.cur_func.vreg_gen,
                    MachineOpcode::Ret,
                    vec![new_op1],
                    &Type::Void,
                    conv_info.cur_bb,
                )))
            }
            DAGNodeKind::CopyToLiveOut => {
                self.convert_dag_to_machine_instr(conv_info, node.operand[0])
            }
            _ => None,
        };

        if machine_instr_id.is_some() {
            self.node_id_to_machine_instr_id
                .insert(node_id, machine_instr_id.unwrap());
        }

        machine_instr_id
    }

    fn convert_call_dag(
        &mut self,
        conv_info: &mut ConversionInfo,
        node: &DAGNode,
    ) -> MachineInstrId {
        fn move_operand_to_reg(
            op: MachineOperand,
            r: MachineRegister,
            bb: MachineBasicBlockId,
        ) -> MachineInstr {
            match op {
                MachineOperand::Branch(_) => unimplemented!(),
                MachineOperand::Constant(_) | MachineOperand::Register(_) => {
                    MachineInstr::new_with_def_reg(mov_rx(&op).unwrap(), vec![op], vec![r], bb)
                }
                MachineOperand::FrameIndex(_) => {
                    MachineInstr::new_with_def_reg(
                        MachineOpcode::LEA64, // TODO
                        vec![op],
                        vec![r],
                        bb,
                    )
                }
                _ => unimplemented!(),
            }
        }

        let mut arg_regs = vec![];
        // TODO: We have to push remaining arguments on stack
        for (i, operand) in node.operand[1..].iter().enumerate() {
            let arg = self.usual_operand(conv_info, *operand);
            let ty = arg.get_type().unwrap();
            let r = RegisterInfo::new_phy_reg(ty, get_arg_reg(i).unwrap())
                .with_vreg(conv_info.cur_func.vreg_gen.next_vreg())
                .into_machine_register();
            arg_regs.push(r.clone());
            let instr = move_operand_to_reg(arg, r, conv_info.cur_bb);
            conv_info.push_instr(instr);
        }

        let callee = self.usual_operand(conv_info, node.operand[0]);

        let ret_reg = RegisterInfo::new_phy_reg(Type::Int32, PhysReg(0))
            .with_vreg(conv_info.cur_func.vreg_gen.next_vreg())
            .into_machine_register(); // i.g. EAX
                                      // TODO: support types other than int.
                                      //       what about struct or union? they won't be assigned to register.

        let call_instr = conv_info.push_instr(
            MachineInstr::new_with_imp_def_use(
                MachineOpcode::Call,
                vec![callee],
                if node.ty == Type::Void {
                    vec![]
                } else {
                    vec![ret_reg.clone()]
                },
                vec![], // TODO: imp-use
                conv_info.cur_bb,
            )
            .with_imp_uses(arg_regs),
        );

        if node.ty == Type::Void {
            call_instr
        } else {
            let ret_reg_ty = ret_reg.info_ref().ty.clone();

            conv_info.push_instr(MachineInstr::new(
                &conv_info.cur_func.vreg_gen,
                MachineOpcode::Copy,
                vec![MachineOperand::Register(ret_reg.clone())],
                &ret_reg_ty,
                conv_info.cur_bb,
            ))
        }
    }

    fn usual_operand(&mut self, conv_info: &mut ConversionInfo, id: DAGNodeId) -> MachineOperand {
        let node = &conv_info.cur_func.dag_arena[id];
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
            _ => MachineOperand::Register(self.convert_dag(conv_info, id).unwrap()),
        }
    }

    fn get_machine_bb(&self, dag_bb_id: DAGBasicBlockId) -> MachineBasicBlockId {
        *self.dag_bb_to_machine_bb.get(&dag_bb_id).unwrap()
    }
}

impl<'a> ConversionInfo<'a> {
    pub fn push_instr(&mut self, instr: MachineInstr) -> MachineInstrId {
        let instr_id = self.machine_instr_arena.alloc(instr);
        self.iseq.push(instr_id);
        instr_id
    }
}

pub fn mov_n_rx(bit: usize, x: &MachineOperand) -> Option<MachineOpcode> {
    // TODO: refine code
    assert!(bit > 0 && ((bit & (bit - 1)) == 0));

    let mov32rx = [MachineOpcode::MOV32rr, MachineOpcode::MOV32ri];
    let xidx = match x {
        MachineOperand::Register(_) => 0,
        MachineOperand::Constant(_) => 1,
        _ => return None, // TODO: Support GlobalAddress?
    };
    match bit {
        32 => Some(mov32rx[xidx]),
        _ => None,
    }
}

pub fn mov_rx(x: &MachineOperand) -> Option<MachineOpcode> {
    let mov32rx = [MachineOpcode::MOV32rr, MachineOpcode::MOV32ri];
    let mov64rx = [MachineOpcode::MOV64rr, MachineOpcode::MOV64ri];
    let (bit, xidx) = match x {
        MachineOperand::Register(r) => (r.info_ref().ty.size_in_bits(), 0),
        MachineOperand::Constant(c) => (c.size_in_bits(), 1),
        _ => return None, // TODO: Support GlobalAddress?
    };
    match bit {
        32 => Some(mov32rx[xidx]),
        64 => Some(mov64rx[xidx]),
        _ => None,
    }
}
