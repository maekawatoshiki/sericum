use super::{node, node::*};
use crate::codegen::arch::machine::abi::AAPCS64;
use crate::codegen::arch::machine::inst::*;
use crate::codegen::arch::machine::register::*;
pub use crate::codegen::common::dag::mc_convert::*;
use crate::codegen::common::machine::calling_conv::ArgumentRegisterOrder;
use crate::codegen::common::machine::inst_def::DefOrUseReg;
use crate::codegen::common::machine::{inst, inst::*, register::*};
use crate::ir::types::*;
use crate::util::allocator::*;

impl<'a> ScheduleByBlock<'a> {
    pub fn convert_node_to_inst(&mut self, node: Raw<DAGNode>) -> MachineInstId {
        if let Some(machine_inst_id) = self.node2inst.get(&node) {
            return *machine_inst_id;
        }

        #[rustfmt::skip]
        macro_rules! cond_kind {($id:expr)=>{ $id.as_cond_kind() };}

        // there should be no NodeKind::IRs here
        let inst_id = match &node.kind {
            NodeKind::MI(_) => {
                fn reg(inst: &MachineInst, x: &DefOrUseReg) -> RegisterOperand {
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
                self.append_inst(inst)
            }
            NodeKind::IR(IRNodeKind::CopyToReg) => {
                let val = self.normal_operand(node.operand[1]);
                let dst = match &node.operand[0].kind {
                    NodeKind::Operand(OperandNodeKind::Register(r)) => RegisterOperand::new(*r),
                    _ => unreachable!(),
                };
                self.append_inst(MachineInst::new_with_def_reg(
                    MachineOpcode::Copy,
                    vec![val],
                    vec![dst],
                    self.cur_bb,
                ))
            }
            NodeKind::IR(IRNodeKind::Call) => self.convert_call_dag(&*node),
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
                self.append_inst(phi_inst)
            }
            NodeKind::IR(IRNodeKind::Brcc) => {
                let op0 = self.normal_operand(node.operand[1]);
                let op1 = self.normal_operand(node.operand[2]);
                let dst =
                    MachineOperand::Branch(self.get_machine_bb(node.operand[3].as_basic_block()));

                self.append_inst(MachineInst::new_simple(
                    if op0.is_register() && op1.is_constant() {
                        MachineOpcode::CMPri
                    } else if op0.is_register() && op1.is_register() {
                        unimplemented!()
                    // MachineOpcode::CMPrr
                    } else {
                        unreachable!()
                    },
                    vec![op0, op1],
                    self.cur_bb,
                ));

                // unimplemented!()
                self.append_inst(MachineInst::new_simple(
                    match cond_kind!(node.operand[0]) {
                        CondKind::Eq => MachineOpcode::B_EQ,
                        CondKind::Ne => MachineOpcode::B_NE,
                        CondKind::Le => MachineOpcode::B_LE,
                        CondKind::Lt => MachineOpcode::B_LT,
                        CondKind::Ge => MachineOpcode::B_GE,
                        CondKind::Gt => MachineOpcode::B_GT,
                        _ => unreachable!(),
                    },
                    vec![dst],
                    self.cur_bb,
                ))
            }
            NodeKind::IR(IRNodeKind::Ret) => self.convert_ret(&*node),
            NodeKind::IR(IRNodeKind::CopyToLiveOut) => self.convert_node_to_inst(node.operand[0]),
            e => panic!("{:?}, {:?}", e, node.ty),
        };

        self.node2inst.insert(node, inst_id);

        inst_id
    }

    fn convert_ret(&mut self, node: &DAGNode) -> MachineInstId {
        let val = self.normal_operand(node.operand[0]);
        if let Some(ty) = val.get_type(&self.cur_func.regs_info) {
            assert!(ty.is_integer());
            let ret_reg = ty2rc(&ty).unwrap().return_value_register();
            let set_ret_val = self.move2reg(self.cur_func.regs_info.get_phys_reg(ret_reg), val);
            self.append_inst(set_ret_val);
        }
        self.append_inst(MachineInst::new_simple(
            MachineOpcode::RET,
            vec![],
            self.cur_bb,
        ))
    }

    fn move2reg(&self, r: RegisterId, src: MachineOperand) -> MachineInst {
        MachineInst::new_simple(opcode_copy2reg(&src), vec![src], self.cur_bb)
            .with_def(vec![RegisterOperand::new(r)])
    }

    fn convert_call_dag(&mut self, node: &DAGNode) -> MachineInstId {
        let mut arg_regs = vec![RegisterOperand::new(
            self.cur_func.regs_info.get_phys_reg(GR64::X30),
        )];
        // let mut off = 0;

        let mut args = vec![];
        for operand in &node.operand[1..] {
            args.push(self.normal_operand(*operand));
        }

        let abi = AAPCS64::new();
        let mut arg_regs_order = ArgumentRegisterOrder::new(&abi);

        for (_, arg) in args.into_iter().enumerate() {
            let ty = arg.get_type(&self.cur_func.regs_info).unwrap();

            if !matches!(
                ty,
                Type::i8 | Type::i32 | Type::i64 | Type::f64 | Type::Pointer(_) | Type::Array(_)
            ) {
                unimplemented!()
            };

            let reg_class = ty2rc(&ty).unwrap();
            let inst = match arg_regs_order.next(reg_class) {
                Some(arg_reg) => {
                    let r = self.cur_func.regs_info.get_phys_reg(arg_reg);
                    arg_regs.push(RegisterOperand::new(r));
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

            self.append_inst(inst);
        }

        // self.append_inst(
        //     MachineInst::new_simple(
        //         MachineOpcode::AdjStackDown,
        //         vec![MachineOperand::imm_i32(off)],
        //         self.cur_bb,
        //     )
        //     .with_imp_def(self.cur_func.regs_info.get_phys_reg(GR64::RSP))
        //     .with_imp_use(self.cur_func.regs_info.get_phys_reg(GR64::RSP)),
        // );

        let callee = self.normal_operand(node.operand[0]);
        let ret_reg = RegisterOperand::new(
            self.cur_func.regs_info.get_phys_reg(
                ty2rc(&node.ty)
                    .unwrap_or(RegisterClassKind::GR64)
                    .return_value_register(),
            ),
        );
        let call_inst = self.append_inst(
            MachineInst::new_simple(MachineOpcode::CALL, vec![callee], self.cur_bb)
                .with_imp_uses(arg_regs)
                .with_imp_defs({
                    let imp_defs = match node.ty {
                        Type::Void => vec![],
                        _ => vec![ret_reg],
                    };
                    imp_defs
                }),
        );

        // self.append_inst(
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

        let reg_class = self.cur_func.regs_info.arena_ref()[ret_reg.id].reg_class;
        let copy = MachineInst::new(
            &self.cur_func.regs_info,
            MachineOpcode::Copy,
            vec![MachineOperand::Register(ret_reg)],
            Some(reg_class),
            self.cur_bb,
        );
        self.append_inst(copy)
    }

    pub fn normal_operand(&mut self, node: Raw<DAGNode>) -> MachineOperand {
        match node.kind {
            NodeKind::Operand(OperandNodeKind::Constant(c)) => match c {
                ConstantKind::Int8(i) => MachineOperand::Constant(MachineConstant::Int8(i)),
                ConstantKind::Int32(i) => MachineOperand::Constant(MachineConstant::Int32(i)),
                ConstantKind::Int64(i) => MachineOperand::Constant(MachineConstant::Int64(i)),
                ConstantKind::F64(f) => MachineOperand::Constant(MachineConstant::F64(f)),
                ConstantKind::Other(c) => todo!(),
            },
            NodeKind::Operand(OperandNodeKind::FrameIndex(ref kind)) => {
                MachineOperand::FrameIndex(kind.clone())
            }
            NodeKind::Operand(OperandNodeKind::Address(ref g)) => match g {
                node::AddressKind::FunctionName(n) => MachineOperand::Mem(
                    MachineMemOperand::Address(inst::AddressKind::FunctionName(n.clone())),
                ),
                _ => unimplemented!(),
            },
            NodeKind::Operand(OperandNodeKind::BasicBlock(bb)) => {
                MachineOperand::Branch(self.get_machine_bb(bb))
            }
            NodeKind::Operand(OperandNodeKind::Register(ref r)) => {
                MachineOperand::Register(RegisterOperand::new(*r))
            }
            NodeKind::Operand(OperandNodeKind::Mem(ref mem)) => match mem {
                MemNodeKind::Reg => MachineOperand::Mem(MachineMemOperand::Reg(
                    *self.normal_operand(node.operand[0]).as_register(),
                )),
                MemNodeKind::RegFi => MachineOperand::Mem(MachineMemOperand::RegFi(
                    *self.normal_operand(node.operand[0]).as_register(),
                    *self.normal_operand(node.operand[1]).as_frame_index(),
                )),
                MemNodeKind::Address => MachineOperand::Mem(MachineMemOperand::Address(
                    inst::AddressKind::Global(*node.operand[0].as_address().as_global()),
                )),
            },
            NodeKind::None => MachineOperand::None,
            _ => MachineOperand::Register(self.convert(node).unwrap()),
        }
    }
}

pub fn opcode_copy2reg(src: &MachineOperand) -> MachineOpcode {
    // unimplemented!()
    match src {
        MachineOperand::Constant(MachineConstant::Int32(_))
        // | MachineOperand::Constant(MachineConstant::Int64(_))
        | MachineOperand::Constant(MachineConstant::Int8(_)) => MachineOpcode::MOVr32i,
        MachineOperand::Constant(MachineConstant::Int64(_)) => unimplemented!(),
        MachineOperand::Register(_) => MachineOpcode::MOVrr,
        _ => unimplemented!(),
    }
}
