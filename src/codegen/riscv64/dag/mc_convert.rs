// TODO: dirty code

use super::super::machine::register::*;
use super::super::machine::{inst, inst::*};
use crate::codegen::arch::dag::{node, node::*};
use crate::codegen::common::dag::{basic_block::DAGBasicBlockId, function::*, module::*};
use crate::codegen::common::machine::{
    basic_block::*, function::*, inst_def::DefOrUseReg, module::*,
};
use crate::ir::types::Type;
use crate::util::allocator::*;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

pub struct ScheduleByBlock<'a> {
    // types: &'a Types,
    cur_func: &'a DAGFunction,
    machine_inst_arena: &'a mut InstructionArena,
    node_to_reg: FxHashMap<Raw<DAGNode>, Option<RegisterId>>,
    cur_bb: MachineBasicBlockId,
    iseq: &'a mut Vec<MachineInstId>,
    bb_map: &'a FxHashMap<DAGBasicBlockId, MachineBasicBlockId>,
    node2minst: &'a mut FxHashMap<Raw<DAGNode>, MachineInstId>,
}

pub fn convert_module(module: DAGModule) -> MachineModule {
    let mut functions = Arena::new();
    for (_, func) in module.functions {
        functions.alloc(convert_function(/*&module.types,*/ func));
    }
    MachineModule::new(module.name, functions, module.types)
}

pub fn convert_function(/*types: &Types,*/ dag_func: DAGFunction) -> MachineFunction {
    let mut bb_map = FxHashMap::default();
    let mut mbbs = MachineBasicBlocks::new();

    for dag_bb_id in &dag_func.dag_basic_blocks {
        let mbb_id = mbbs.arena.alloc(MachineBasicBlock::new());
        bb_map.insert(*dag_bb_id, mbb_id);
        mbbs.order.push(mbb_id);
    }

    for (dag, machine) in &bb_map {
        let dbb = &dag_func.dag_basic_block_arena[*dag];
        let mbb = &mut mbbs.arena[*machine];
        mbb.pred = dbb.pred.iter().map(|bb| *bb_map.get(bb).unwrap()).collect();
        mbb.succ = dbb.succ.iter().map(|bb| *bb_map.get(bb).unwrap()).collect();
    }

    let mut machine_inst_arena = InstructionArena::new();
    let mut node2minst = FxHashMap::default();

    for dag_bb_id in &dag_func.dag_basic_blocks {
        let node = &dag_func.dag_basic_block_arena[*dag_bb_id];
        let bb_id = *bb_map.get(dag_bb_id).unwrap();
        let mut iseq = vec![];

        let entry = match node.entry {
            Some(entry) => entry,
            None => continue,
        };

        ScheduleByBlock {
            // types,
            cur_func: &dag_func,
            machine_inst_arena: &mut machine_inst_arena,
            node_to_reg: FxHashMap::default(),
            cur_bb: bb_id,
            iseq: &mut iseq,
            bb_map: &bb_map,
            node2minst: &mut node2minst,
        }
        .convert(entry);

        mbbs.arena[bb_id].iseq = RefCell::new(iseq);
    }

    MachineFunction::new(dag_func, mbbs, machine_inst_arena)
}

impl<'a> ScheduleByBlock<'a> {
    pub fn convert(&mut self, node: Raw<DAGNode>) -> Option<RegisterId> {
        if let Some(reg_id) = self.node_to_reg.get(&node) {
            return *reg_id;
        }

        let reg_id = match self.convert_node_to_inst(node) {
            Some(id) => {
                let inst = &self.machine_inst_arena[id];
                inst.get_def_reg()
            }
            None => None,
        };
        self.node_to_reg.insert(node, reg_id);

        some_then!(next, node.next, {
            self.convert(next);
        });

        reg_id
    }

    fn convert_node_to_inst(&mut self, node: Raw<DAGNode>) -> Option<MachineInstId> {
        if let Some(machine_inst_id) = self.node2minst.get(&node) {
            return Some(*machine_inst_id);
        }

        #[rustfmt::skip]
        macro_rules! cond_kind {($id:expr)=>{ $id.as_cond_kind() };}

        // there should be no NodeKind::IRs here
        let machine_inst_id = match &node.kind {
            NodeKind::MI(_) => {
                fn reg(inst: &MachineInst, x: &DefOrUseReg) -> RegisterId {
                    match x {
                        DefOrUseReg::Def(i) => inst.def[*i],
                        DefOrUseReg::Use(i) => *inst.operand[*i].as_register(),
                    }
                }
                let mi = node.kind.as_mi();
                let inst_def = mi.inst_def().unwrap();
                let operands = node
                    .operand
                    .iter()
                    .map(|op| self.normal_operand(*op))
                    .collect();
                let mut inst = MachineInst::new(
                    &self.cur_func.regs_info,
                    mi,
                    operands,
                    ty2rc(&node.ty),
                    self.cur_bb,
                );
                for (def_, use_) in &inst_def.tie {
                    inst.tie_regs(reg(&inst, def_), reg(&inst, use_));
                }
                Some(self.push_inst(inst))
            }
            NodeKind::IR(IRNodeKind::Entry) => None,
            NodeKind::IR(IRNodeKind::CopyToReg) => {
                let val = self.normal_operand(node.operand[1]);
                let dst = match &node.operand[0].kind {
                    NodeKind::Operand(OperandNodeKind::Register(r)) => *r,
                    _ => unreachable!(),
                };
                Some(self.push_inst(MachineInst::new_with_def_reg(
                    MachineOpcode::Copy,
                    vec![val],
                    vec![dst],
                    self.cur_bb,
                )))
            }
            NodeKind::IR(IRNodeKind::Call) => Some(self.convert_call_dag(&*node)),
            NodeKind::IR(IRNodeKind::Phi) => {
                let mut operands = vec![];
                let mut i = 0;
                while i < node.operand.len() {
                    operands.push(self.normal_operand(node.operand[i]));
                    operands.push(MachineOperand::Branch(
                        self.get_machine_bb(node.operand[i + 1].as_basic_block()),
                    ));
                    i += 2;
                }
                let phi_inst = MachineInst::new(
                    &self.cur_func.regs_info,
                    MachineOpcode::Phi,
                    operands,
                    ty2rc(&node.ty),
                    self.cur_bb,
                );
                Some(self.push_inst(phi_inst))
            }
            // NodeKind::IR(IRNodeKind::Setcc) => {
            //     let new_op1 = self.normal_operand(node.operand[1]);
            //     let new_op2 = self.normal_operand(node.operand[2]);
            //     let inst = MachineInst::new(
            //         &self.cur_func.regs_info,
            //         match cond_kind!(node.operand[0]) {
            //             CondKind::Eq => MachineOpcode::Seteq,
            //             CondKind::Le => MachineOpcode::Setle,
            //             CondKind::Lt => MachineOpcode::Setlt,
            //             _ => unimplemented!(),
            //         },
            //         vec![new_op1, new_op2],
            //         ty2rc(&node.ty),
            //         self.cur_bb,
            //     );
            //     Some(self.push_inst(inst))
            // }
            // NodeKind::IR(IRNodeKind::Br) => {
            //     let inst = MachineInst::new(
            //         &self.cur_func.regs_info,
            //         MachineOpcode::JMP,
            //         vec![MachineOperand::Branch(
            //             self.get_machine_bb(bb!(node.operand[0])),
            //         )],
            //         None,
            //         self.cur_bb,
            //     );
            //     Some(self.push_inst(inst))
            // }
            // NodeKind::IR(IRNodeKind::BrCond) => {
            //     let new_cond = self.normal_operand(node.operand[0]);
            //     let inst = MachineInst::new(
            //         &self.cur_func.regs_info,
            //         MachineOpcode::BrCond,
            //         vec![
            //             new_cond,
            //             MachineOperand::Branch(self.get_machine_bb(bb!(node.operand[1]))),
            //         ],
            //         None,
            //         self.cur_bb,
            //     );
            //     Some(self.push_inst(inst))
            // }
            NodeKind::IR(IRNodeKind::Brcc) => {
                let mut operands = node.operand[1..3]
                    .iter()
                    .map(|&op| {
                        let op = self.normal_operand(op);
                        match self.move2reg_if_necessary(op) {
                            Ok((r, inst)) => {
                                self.push_inst(inst);
                                MachineOperand::Register(r)
                            }
                            Err(op) => op,
                        }
                    })
                    .collect::<Vec<_>>();
                operands.push(MachineOperand::Branch(
                    self.get_machine_bb(node.operand[3].as_basic_block()),
                ));
                Some(self.push_inst(MachineInst::new_simple(
                    match cond_kind!(node.operand[0]) {
                        CondKind::Eq => MachineOpcode::BEQ,
                        CondKind::Le => MachineOpcode::BLE,
                        // CondKind::Lt => MachineOpcode::JL,
                        // CondKind::Ge => MachineOpcode::JGE,
                        // CondKind::Gt => MachineOpcode::JG,
                        _ => unreachable!(),
                    },
                    operands,
                    self.cur_bb,
                )))
            }
            // NodeKind::IR(IRNodeKind::FPBrcc) => {
            //     let op0 = self.normal_operand(node.operand[1]);
            //     let op1 = self.normal_operand(node.operand[2]);
            //
            //     self.push_inst(MachineInst::new_simple(
            //         MachineOpcode::UCOMISDrr,
            //         vec![op0, op1],
            //         self.cur_bb,
            //     ));
            //
            //     Some(self.push_inst(MachineInst::new_simple(
            //         match cond_kind!(node.operand[0]) {
            //             CondKind::UEq => MachineOpcode::JE,
            //             CondKind::ULe => MachineOpcode::JBE,
            //             CondKind::ULt => MachineOpcode::JB,
            //             CondKind::UGe => MachineOpcode::JAE,
            //             CondKind::UGt => MachineOpcode::JA,
            //             _ => unreachable!(),
            //         },
            //         vec![MachineOperand::Branch(
            //             self.get_machine_bb(bb!(node.operand[3])),
            //         )],
            //         self.cur_bb,
            //     )))
            // }
            NodeKind::IR(IRNodeKind::Ret) => Some(self.convert_ret(&*node)),
            NodeKind::IR(IRNodeKind::CopyToLiveOut) => self.convert_node_to_inst(node.operand[0]),
            e => {
                println!("{:?}", e);
                // println!("{:?}, {:?}", *node.operand[0], *node.operand[1]);
                None
            }
        };

        if machine_inst_id.is_some() {
            self.node2minst.insert(node, machine_inst_id.unwrap());
        }

        machine_inst_id
    }

    fn convert_ret(&mut self, node: &DAGNode) -> MachineInstId {
        let val = self.normal_operand(node.operand[0]);
        let ra = self.cur_func.regs_info.get_phys_reg(GPR::RA);

        if let Some(ty) = val.get_type(&self.cur_func.regs_info) {
            assert!(ty.is_integer());
            let ret_reg = ty2rc(&ty).unwrap().return_value_register();
            let set_ret_val = self.move2reg(self.cur_func.regs_info.get_phys_reg(ret_reg), val);
            self.push_inst(set_ret_val);
        }

        self.push_inst(MachineInst::new_simple(
            MachineOpcode::JR, // TODO
            vec![MachineOperand::Register(ra)],
            self.cur_bb,
        ))
    }

    fn move2reg(&self, r: RegisterId, src: MachineOperand) -> MachineInst {
        MachineInst::new_simple(opcode_copy2reg(&src), vec![src], self.cur_bb).with_def(vec![r])
    }

    fn move2reg_if_necessary(
        &self,
        src: MachineOperand,
    ) -> Result<(RegisterId, MachineInst), MachineOperand> {
        match src {
            MachineOperand::Register(_) => Err(src),
            _ => {
                let r = self
                    .cur_func
                    .regs_info
                    .new_virt_reg(ty2rc(&src.get_type(&self.cur_func.regs_info).unwrap()).unwrap());
                Ok((
                    r,
                    MachineInst::new_simple(opcode_copy2reg(&src), vec![src], self.cur_bb)
                        .with_def(vec![r]),
                ))
            }
        }
    }

    fn convert_call_dag(&mut self, node: &DAGNode) -> MachineInstId {
        let mut arg_regs = vec![];
        // let mut off = 0;

        let mut args = vec![];
        for operand in &node.operand[1..] {
            args.push(self.normal_operand(*operand));
        }

        for (i, arg) in args.into_iter().enumerate() {
            println!(">>> {:?}", arg);
            let ty = arg.get_type(&self.cur_func.regs_info).unwrap();

            if !matches!(
                ty,
                Type::Int8 | Type::Int32 | Type::Int64 | Type::F64 | Type::Pointer(_) | Type::Array(_)
            ) {
                unimplemented!()
            };

            let reg_class = ty2rc(&ty).unwrap();
            let inst = match reg_class.get_nth_arg_reg(i) {
                Some(arg_reg) => {
                    let r = self.cur_func.regs_info.get_phys_reg(arg_reg);
                    arg_regs.push(r.clone());
                    self.move2reg(r, arg)
                }
                None => {
                    // Put the exceeded value onto the stack
                    // let inst = MachineInst::new_simple(
                    //     mov_mx(&self.cur_func.regs_info, &arg).unwrap(),
                    //     vec![
                    //         MachineOperand::Mem(MachineMemOperand::BaseOff(
                    //             self.cur_func.regs_info.get_phys_reg(GR64::RSP),
                    //             off,
                    //         )),
                    //         arg,
                    //     ],
                    //     self.cur_bb,
                    // );
                    // off += 8;
                    // inst
                    unimplemented!()
                }
            };

            println!("{:?}", inst);
            self.push_inst(inst);
        }

        // self.push_inst(
        //     MachineInst::new_simple(
        //         MachineOpcode::AdjStackDown,
        //         vec![MachineOperand::imm_i32(off)],
        //         self.cur_bb,
        //     )
        //     .with_imp_def(self.cur_func.regs_info.get_phys_reg(GR64::RSP))
        //     .with_imp_use(self.cur_func.regs_info.get_phys_reg(GR64::RSP)),
        // );

        let callee = self.normal_operand(node.operand[0]);
        let ret_reg = self.cur_func.regs_info.get_phys_reg(
            ty2rc(&node.ty)
                .unwrap_or(RegisterClassKind::GPR)
                .return_value_register(),
        );
        let call_inst = self.push_inst(
            MachineInst::new_simple(MachineOpcode::CALL, vec![callee], self.cur_bb)
                .with_imp_uses(arg_regs)
                .with_imp_defs({
                    let mut imp_defs = match node.ty {
                        Type::Void => vec![],
                        _ => vec![ret_reg],
                    };
                    imp_defs.push(self.cur_func.regs_info.get_phys_reg(GPR::RA));
                    imp_defs
                }),
        );

        // self.push_inst(
        //     MachineInst::new_simple(
        //         MachineOpcode::AdjStackUp,
        //         vec![MachineOperand::imm_i32(off)],
        //         self.cur_bb,
        //     )
        //     .with_imp_def(self.cur_func.regs_info.get_phys_reg(GR64::RSP))
        //     .with_imp_use(self.cur_func.regs_info.get_phys_reg(GR64::RSP)),
        // );

        if node.ty == Type::Void {
            return call_inst;
        }

        let reg_class = self.cur_func.regs_info.arena_ref()[ret_reg].reg_class;
        let copy = MachineInst::new(
            &self.cur_func.regs_info,
            MachineOpcode::Copy,
            vec![MachineOperand::Register(ret_reg)],
            Some(reg_class),
            self.cur_bb,
        );
        self.push_inst(copy)
    }

    fn normal_operand(&mut self, node: Raw<DAGNode>) -> MachineOperand {
        match node.kind {
            NodeKind::Operand(OperandNodeKind::Constant(c)) => match c {
                ConstantKind::Int8(i) => MachineOperand::Constant(MachineConstant::Int8(i)),
                ConstantKind::Int32(i) => MachineOperand::Constant(MachineConstant::Int32(i)),
                ConstantKind::Int64(i) => MachineOperand::Constant(MachineConstant::Int64(i)),
                ConstantKind::F64(f) => MachineOperand::Constant(MachineConstant::F64(f)),
            },
            NodeKind::Operand(OperandNodeKind::FrameIndex(ref kind)) => {
                MachineOperand::FrameIndex(kind.clone())
            }
            NodeKind::Operand(OperandNodeKind::Address(ref g)) => match g {
                node::AddressKind::FunctionName(n) => MachineOperand::Mem(
                    MachineMemOperand::Address(inst::AddressKind::FunctionName(n.clone())),
                ),
            },
            NodeKind::Operand(OperandNodeKind::BasicBlock(bb)) => {
                MachineOperand::Branch(self.get_machine_bb(bb))
            }
            NodeKind::Operand(OperandNodeKind::Register(ref r)) => MachineOperand::Register(*r),
            NodeKind::Operand(OperandNodeKind::Mem(ref mem)) => match mem {
                MemNodeKind::FiReg => MachineOperand::Mem(MachineMemOperand::FiReg(
                    *self.normal_operand(node.operand[0]).as_frame_index(),
                    *self.normal_operand(node.operand[1]).as_register(),
                )),
                MemNodeKind::ImmReg => MachineOperand::Mem(MachineMemOperand::ImmReg(
                        self.normal_operand(node.operand[0]).as_constant().as_i32(),
                        *self.normal_operand(node.operand[1]).as_register()
                ))
                // MemNodeKind::Base => MachineOperand::Mem(MachineMemOperand::Base(
                //     *self.normal_operand(node.operand[0]).as_register(),
                // )),
                // MemNodeKind::BaseAlignOff => MachineOperand::Mem(MachineMemOperand::BaseAlignOff(
                //     *self.normal_operand(node.operand[0]).as_register(),
                //     self.normal_operand(node.operand[1]).as_constant().as_i32(),
                //     *self.normal_operand(node.operand[2]).as_register(),
                // )),
                // MemNodeKind::BaseFi => MachineOperand::Mem(MachineMemOperand::BaseFi(
                //     *self.normal_operand(node.operand[0]).as_register(),
                //     *self.normal_operand(node.operand[1]).as_frame_index(),
                // )),
                // MemNodeKind::BaseFiAlignOff => {
                //     MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(
                //         *self.normal_operand(node.operand[0]).as_register(),
                //         *self.normal_operand(node.operand[1]).as_frame_index(),
                //         self.normal_operand(node.operand[2]).as_constant().as_i32(),
                //         *self.normal_operand(node.operand[3]).as_register(),
                //     ))
                // }
                // MemNodeKind::BaseFiOff => MachineOperand::Mem(MachineMemOperand::BaseFiOff(
                //     *self.normal_operand(node.operand[0]).as_register(),
                //     *self.normal_operand(node.operand[1]).as_frame_index(),
                //     self.normal_operand(node.operand[2]).as_constant().as_i32(),
                // )),
            },
            NodeKind::None => MachineOperand::None,
            _ => MachineOperand::Register(self.convert(node).unwrap()),
        }
    }

    fn get_machine_bb(&self, dag_bb_id: DAGBasicBlockId) -> MachineBasicBlockId {
        *self.bb_map.get(&dag_bb_id).unwrap()
    }

    pub fn push_inst(&mut self, inst: MachineInst) -> MachineInstId {
        let inst_id = self
            .machine_inst_arena
            .alloc(&self.cur_func.regs_info, inst);
        self.iseq.push(inst_id);
        inst_id
    }
}

// pub fn mov_n_rx(bit: usize, x: &MachineOperand) -> Option<MachineOpcode> {
//     // TODO: refine code
//     assert!(bit > 0 && ((bit & (bit - 1)) == 0));
//
//     let mov32rx = [MachineOpcode::MOVrr32, MachineOpcode::MOVri32];
//     let xidx = match x {
//         MachineOperand::Register(_) => 0,
//         MachineOperand::Constant(_) => 1,
//         _ => return None, // TODO: Support Address?
//     };
//     match bit {
//         32 => Some(mov32rx[xidx]),
//         _ => None,
//     }
// }

// pub fn mov_rx(tys: &Types, regs_info: &RegistersInfo, x: &MachineOperand) -> Option<MachineOpcode> {
//     // TODO: special handling for float
//     if x.get_type(regs_info).unwrap() == Type::F64 {
//         return match x {
//             MachineOperand::Constant(_) => Some(MachineOpcode::MOVSDrm64),
//             MachineOperand::FrameIndex(_) | MachineOperand::Mem(_) => Some(MachineOpcode::MOVSDrm),
//             MachineOperand::Register(_) => Some(MachineOpcode::MOVSDrr),
//             _ => None,
//         };
//     }
//
//     let mov32rx = [
//         MachineOpcode::MOVrr32,
//         MachineOpcode::MOVri32,
//         MachineOpcode::MOVrm32,
//     ];
//     let mov64rx = [
//         MachineOpcode::MOVrr64,
//         MachineOpcode::MOVri64,
//         MachineOpcode::MOVrm64,
//     ];
//     let (bit, xidx) = match x {
//         MachineOperand::Register(r) => (regs_info.arena_ref()[*r].reg_class.size_in_bits(), 0),
//         MachineOperand::Constant(c) => (c.size_in_bits(), 1),
//         MachineOperand::FrameIndex(f) => (f.ty.size_in_bits(tys), 2),
//         _ => return None, // TODO: Support Address?
//     };
//     match bit {
//         32 => Some(mov32rx[xidx]),
//         64 => Some(mov64rx[xidx]),
//         _ => None,
//     }
// }
//
// pub fn mov_mx(regs_info: &RegistersInfo, x: &MachineOperand) -> Option<MachineOpcode> {
//     if x.get_type(regs_info).unwrap() == Type::F64 {
//         return match x {
//             MachineOperand::Register(_) => Some(MachineOpcode::MOVSDmr),
//             _ => None,
//         };
//     }
//
//     let mov32mx = [MachineOpcode::MOVmr32, MachineOpcode::MOVmi32];
//     let mov64mx = [MachineOpcode::MOVmr64, MachineOpcode::MOVmi64];
//     // let mov64rx = [
//     //     MachineOpcode::MOVrr64,
//     //     MachineOpcode::MOVri64,
//     //     MachineOpcode::MOVrm64,
//     // ];
//     let (bit, n) = match x {
//         MachineOperand::Register(r) => (regs_info.arena_ref()[*r].reg_class.size_in_bits(), 0),
//         MachineOperand::Constant(c) => (c.size_in_bits(), 1),
//         _ => return None, // TODO: Support Address?
//     };
//     match bit {
//         32 => Some(mov32mx[n]),
//         64 => Some(mov64mx[n]),
//         _ => None,
//     }
// }

pub fn opcode_copy2reg(src: &MachineOperand) -> MachineOpcode {
    match src {
        MachineOperand::Constant(MachineConstant::Int32(_))
        | MachineOperand::Constant(MachineConstant::Int64(_))
        | MachineOperand::Constant(MachineConstant::Int8(_)) => MachineOpcode::LI,
        MachineOperand::Register(_) => MachineOpcode::MV,
        _ => unimplemented!(),
    }
}
