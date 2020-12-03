// use super::{node, node::*};
use crate::codegen::arch::dag::node::MemKind;
use crate::codegen::arch::frame_object::FrameIndexInfo;
use crate::codegen::arch::machine::abi::SystemV;
use crate::codegen::arch::machine::inst::*;
use crate::codegen::arch::machine::register::*;
pub use crate::codegen::common::dag::mc_convert::ScheduleContext;
use crate::codegen::common::dag::{
    node,
    node::{CondKind, IRNode, IROpcode, ImmediateKind, MINode, Node, NodeId, OperandNode},
};
use crate::codegen::common::machine::calling_conv::{ArgumentRegisterOrder, CallingConv};
use crate::codegen::common::machine::inst::*;
use crate::codegen::common::machine::inst_def::DefOrUseReg;
use crate::codegen::common::types::MVType;
use crate::ir::types::Type;

impl<'a> ScheduleContext<'a> {
    pub fn convert_node(&mut self, id: NodeId) -> MachineInstId {
        if let Some(inst_id) = self.node2inst.get(&id) {
            return *inst_id;
        }

        let inst_id = match &self.func.node_arena[id] {
            Node::MI(MINode {
                opcode,
                args,
                reg_class,
                ..
            }) => {
                fn reg(inst: &MachineInst, x: &DefOrUseReg) -> RegisterOperand {
                    match x {
                        DefOrUseReg::Def(i) => inst.def[*i],
                        DefOrUseReg::Use(i) => *inst.operand[*i].as_register(),
                    }
                }
                let inst_def = opcode.inst_def().unwrap();
                let operands = args.iter().map(|op| self.normal_arg(*op)).collect();
                let mut inst = MachineInst::new(
                    &self.func.regs,
                    *opcode,
                    operands,
                    *reg_class,
                    self.block_id,
                );
                for (def_, use_) in &inst_def.tie {
                    inst.tie_regs(reg(&inst, def_), reg(&inst, use_));
                }
                self.append_inst(inst)
            }
            Node::IR(IRNode {
                opcode: IROpcode::CopyToReg,
                args,
                ..
            }) => {
                let src = self.normal_arg(args[1]);
                let dst = match &self.func.node_arena[args[0]] {
                    Node::Operand(OperandNode::Reg(r)) => RegisterOperand::new(*r),
                    _ => unreachable!(),
                };
                self.append_inst(MachineInst::new_with_def_reg(
                    MachineOpcode::Copy,
                    vec![src],
                    vec![dst],
                    self.block_id,
                ))
            }
            Node::IR(IRNode {
                opcode: IROpcode::Setcc,
                args,
                ..
            }) => {
                let lhs = self.normal_arg(args[1]);
                let rhs = self.normal_arg(args[2]);

                self.append_inst(MachineInst::new_simple(
                    if lhs.is_register() && rhs.is_constant() {
                        MachineOpcode::CMPri
                    } else if lhs.is_register() && rhs.is_register() {
                        MachineOpcode::CMPrr
                    } else {
                        unreachable!()
                    },
                    vec![lhs, rhs],
                    self.block_id,
                ));

                let r = RegisterOperand::new(self.func.regs.new_virt_reg(RegisterClassKind::GR8));
                self.append_inst(
                    MachineInst::new_simple(
                        match self.func.node_arena[args[0]].as_operand().as_cc() {
                            CondKind::Eq => MachineOpcode::SETE,
                            CondKind::Ne => MachineOpcode::SETNE,
                            CondKind::Le => MachineOpcode::SETLE,
                            CondKind::Lt => MachineOpcode::SETL,
                            CondKind::Ge => MachineOpcode::SETGE,
                            CondKind::Gt => MachineOpcode::SETG,
                            _ => todo!(),
                        },
                        vec![],
                        self.block_id,
                    )
                    .with_def(vec![r]),
                );

                let copy = MachineInst::new(
                    &self.func.regs,
                    MachineOpcode::Copy,
                    vec![MachineOperand::Register(r)],
                    Some(RegisterClassKind::GR8),
                    self.block_id,
                );
                self.append_inst(copy)
            }
            Node::IR(IRNode {
                opcode: IROpcode::Brcc,
                args,
                ..
            }) => {
                let lhs = self.normal_arg(args[1]);
                let rhs = self.normal_arg(args[2]);

                self.append_inst(MachineInst::new_simple(
                    if lhs.is_register() && rhs.is_constant() {
                        MachineOpcode::CMPri
                    } else if lhs.is_register() && rhs.is_register() {
                        MachineOpcode::CMPrr
                    } else {
                        unreachable!()
                    },
                    vec![lhs, rhs],
                    self.block_id,
                ));

                self.append_inst(MachineInst::new_simple(
                    match self.func.node_arena[args[0]].as_operand().as_cc() {
                        CondKind::Eq => MachineOpcode::JE,
                        CondKind::Ne => MachineOpcode::JNE,
                        CondKind::Le => MachineOpcode::JLE,
                        CondKind::Lt => MachineOpcode::JL,
                        CondKind::Ge => MachineOpcode::JGE,
                        CondKind::Gt => MachineOpcode::JG,
                        _ => unreachable!(),
                    },
                    vec![MachineOperand::Branch(
                        self.bb_map[self.func.node_arena[args[3]].as_operand().as_block()],
                    )],
                    self.block_id,
                ))
            }
            Node::IR(IRNode {
                opcode: IROpcode::FPBrcc,
                args,
                ..
            }) => {
                let lhs = self.normal_arg(args[1]);
                let rhs = self.normal_arg(args[2]);

                self.append_inst(MachineInst::new_simple(
                    MachineOpcode::UCOMISDrr,
                    vec![lhs, rhs],
                    self.block_id,
                ));

                self.append_inst(MachineInst::new_simple(
                    match self.func.node_arena[args[0]].as_operand().as_cc() {
                        CondKind::UEq => MachineOpcode::JE,
                        CondKind::UNe => MachineOpcode::JNE,
                        CondKind::ULe => MachineOpcode::JBE,
                        CondKind::ULt => MachineOpcode::JB,
                        CondKind::UGe => MachineOpcode::JAE,
                        CondKind::UGt => MachineOpcode::JA,
                        _ => unreachable!(),
                    },
                    vec![MachineOperand::Branch(
                        self.bb_map[self.func.node_arena[args[3]].as_operand().as_block()],
                    )],
                    self.block_id,
                ))
            }
            Node::IR(IRNode {
                opcode: IROpcode::Div,
                args,
                mvty,
                ..
            }) => self.convert_div(*mvty, args, false),
            Node::IR(IRNode {
                opcode: IROpcode::Rem,
                args,
                mvty,
                ..
            }) => self.convert_div(*mvty, args, true),
            Node::IR(IRNode {
                opcode: IROpcode::Call,
                args,
                ty,
                ..
            }) => self.convert_call(*ty, args),
            Node::IR(IRNode {
                opcode: IROpcode::Phi,
                args,
                ty,
                ..
            }) => {
                let mut operands = vec![];
                for i in (0..args.len()).step_by(2) {
                    let val = args[i];
                    let block = args[i + 1];
                    operands.push(self.normal_arg(val));
                    operands.push(self.normal_arg(block));
                }
                let phi_inst = MachineInst::new(
                    &self.func.regs,
                    MachineOpcode::Phi,
                    operands,
                    ty2rc(ty),
                    self.block_id,
                );
                self.append_inst(phi_inst)
            }
            Node::IR(IRNode {
                opcode: IROpcode::Ret,
                args,
                ..
            }) => self.convert_ret(args[0]),
            e => todo!("{:?}", e),
        };
        inst_id
    }

    fn convert_div(&mut self, mvty: MVType, args: &[NodeId], is_rem: bool) -> MachineInstId {
        let regs = match mvty {
            MVType::i8 => to_phys!(GR32::EAX, GR32::EDX),
            MVType::i32 => to_phys!(GR32::EAX, GR32::EDX),
            _ => todo!(),
        };
        let (eax, edx) = (
            RegisterOperand::new(self.func.regs.get_phys_reg(regs[0])),
            RegisterOperand::new(self.func.regs.get_phys_reg(regs[1])),
        );
        let (mut lhs, mut rhs) = (self.normal_arg(args[0]), self.normal_arg(args[1]));
        if mvty == MVType::i8 {
            if let MachineOperand::Register(r) = &mut lhs {
                *r = r.sub_super(Some(RegisterClassKind::GR32))
            }
            if let MachineOperand::Register(r) = &mut rhs {
                *r = r.sub_super(Some(RegisterClassKind::GR32))
            }
        }
        self.append_inst(
            MachineInst::new_simple(
                mov_rx(regs[0].reg_class(), &lhs).unwrap(),
                vec![lhs],
                self.block_id,
            )
            .with_def(vec![eax]),
        );
        self.append_inst(
            MachineInst::new_simple(MachineOpcode::CDQ, vec![], self.block_id)
                .with_imp_defs(vec![eax, edx])
                .with_imp_use(eax),
        );
        let mov = MachineInst::new(
            &self.func.regs,
            mov_rx(regs[0].reg_class(), &rhs).unwrap(),
            vec![rhs],
            Some(regs[0].reg_class()),
            self.block_id,
        );
        let rhs = MachineOperand::Register(mov.def[0]);
        self.append_inst(mov);
        self.append_inst(
            MachineInst::new_simple(MachineOpcode::IDIV, vec![rhs], self.block_id)
                .with_imp_defs(vec![eax, edx])
                .with_imp_uses(vec![eax, edx]),
        );
        let copy = MachineInst::new(
            &self.func.regs,
            MachineOpcode::Copy,
            vec![MachineOperand::Register(if is_rem { edx } else { eax })],
            Some(regs[0].reg_class()),
            self.block_id,
        );
        self.append_inst(copy)
    }

    fn convert_call(&mut self, ret_ty: Type, operands: &[NodeId]) -> MachineInstId {
        let mut arg_regs = vec![RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RSP))]; // call uses RSP
        let mut off = 0i32;

        let func_name = self.func.node_arena[operands[0]]
            .as_operand()
            .as_addr()
            .as_func_name();
        let func_params: Vec<(Type, bool)> = {
            let func_ty = self.func.types.compound_ty(self.func_map[func_name]);
            let func_ty = func_ty.as_function();
            func_ty
                .params_ty
                .iter()
                .enumerate()
                .map(|(i, ty)| (*ty, func_ty.params_attr.get(&i).map_or(false, |a| a.byval)))
                .collect()
        };

        let mut args = vec![];
        for (i, arg) in operands[1..].iter().enumerate() {
            let byval = func_params[i].1;
            args.push(if byval {
                MachineOperand::None
            } else {
                self.normal_arg(*arg)
            });
        }

        let abi = SystemV::new();
        let mut arg_regs_order = ArgumentRegisterOrder::new(&abi);

        for (i, arg) in args.into_iter().enumerate() {
            let (ty, byval) = func_params[i];

            if byval {
                // TODO
                let lea = &self.func.node_arena[operands[1 + i]];
                let mem = lea.as_mi().args[0];
                let fi = match self.normal_arg(mem) {
                    MachineOperand::Mem(MachineMemOperand::BaseFi(_, fi)) => fi,
                    _ => panic!(),
                };
                arg_regs.append(&mut self.pass_struct_byval(&mut arg_regs_order, &mut off, ty, fi));
                continue;
            }

            if !matches!(
                ty,
                Type::i8 | Type::i32 | Type::i64 | Type::f64 | Type::Pointer(_) | Type::Array(_)
            ) {
                unimplemented!()
            }

            let reg_class = ty2rc(&ty).unwrap();
            let inst = match arg_regs_order.next(reg_class) {
                Some(arg_reg) => {
                    let r = self.func.regs.get_phys_reg(arg_reg);
                    arg_regs.push(RegisterOperand::new(r));
                    MachineInst::new_simple(
                        mov_rx(arg_reg.reg_class(), &arg).unwrap(),
                        vec![arg],
                        self.block_id,
                    )
                    .with_def(vec![RegisterOperand::new(r)])
                }
                None => {
                    // Put the exceeded value onto the stack
                    let inst = MachineInst::new_simple(
                        mov_mx(&self.func.regs, &arg).unwrap(),
                        vec![
                            MachineOperand::Mem(MachineMemOperand::BaseOff(
                                RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RSP)),
                                off,
                            )),
                            arg,
                        ],
                        self.block_id,
                    );
                    off += 8;
                    inst
                }
            };

            self.append_inst(inst);
        }

        self.append_inst(
            MachineInst::new_simple(
                MachineOpcode::AdjStackDown,
                vec![MachineOperand::imm_i32(off)],
                self.block_id,
            )
            .with_imp_def(RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RSP)))
            .with_imp_use(RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RSP))),
        );

        let callee = self.normal_arg(operands[0]);
        let ret_reg = self.func.regs.get_phys_reg(
            ty2rc(&ret_ty)
                .unwrap_or(RegisterClassKind::GR32)
                .return_value_register(),
        );
        let call_inst = self.append_inst(
            MachineInst::new_simple(MachineOpcode::CALL, vec![callee], self.block_id)
                .with_imp_uses(arg_regs)
                .with_imp_defs({
                    let mut defs =
                        vec![RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RSP))];
                    if ret_ty != Type::Void {
                        defs.push(RegisterOperand::new(ret_reg));
                    }
                    defs
                }),
        );

        self.append_inst(
            MachineInst::new_simple(
                MachineOpcode::AdjStackUp,
                vec![MachineOperand::imm_i32(off)],
                self.block_id,
            )
            .with_imp_def(RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RSP)))
            .with_imp_use(RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RSP))),
        );

        if ret_ty == Type::Void {
            return call_inst;
        }

        let ret_reg_class = self.func.regs.arena_ref()[ret_reg].reg_class;
        let copy = MachineInst::new(
            &self.func.regs,
            MachineOpcode::Copy,
            vec![MachineOperand::Register(RegisterOperand::new(ret_reg))],
            Some(ret_reg_class),
            self.block_id,
        );
        self.append_inst(copy)
    }

    fn pass_struct_byval<ABI>(
        &mut self,
        arg_regs_order: &mut ArgumentRegisterOrder<ABI>,
        off: &mut i32,
        ty: Type,
        fi: FrameIndexInfo,
    ) -> Vec<RegisterOperand>
    where
        ABI: CallingConv,
    {
        let mut arg_regs = vec![];
        let struct_ty = self.func.types.get_element_ty(ty, None).unwrap();
        let base = &self.func.types.base.borrow();
        let struct_ty = base.as_struct_ty(struct_ty).unwrap();
        let sz = struct_ty.size();
        let mov8 = sz / 8;
        let mov4 = (sz - 8 * mov8) / 4;
        let rbp = RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RBP));
        assert!((sz - 8 * mov8) % 4 == 0);
        let regs_classes = SystemV::reg_classes_used_for_passing_byval(struct_ty);

        if sz <= 16 && arg_regs_order.regs_available_for(&regs_classes) {
            let mut off = 0;
            for &rc in &regs_classes {
                let r = RegisterOperand::new(
                    self.func
                        .regs
                        .get_phys_reg(arg_regs_order.next(rc).unwrap()),
                );
                arg_regs.push(r);

                let mem = MachineOperand::Mem(if off == 0 {
                    MachineMemOperand::BaseFi(rbp, fi.clone())
                } else {
                    MachineMemOperand::BaseFiOff(rbp, fi.clone(), off as i32)
                });

                let mov = MachineInst::new_simple(
                    match rc {
                        RegisterClassKind::GR32 => MachineOpcode::MOVrm32,
                        RegisterClassKind::GR64 => MachineOpcode::MOVrm64,
                        RegisterClassKind::XMM => MachineOpcode::MOVSDrm,
                        RegisterClassKind::GR8 => unimplemented!(),
                    },
                    vec![mem],
                    self.block_id,
                )
                .with_def(vec![r]);
                self.append_inst(mov);
                off += match rc {
                    RegisterClassKind::XMM => 8,
                    _ => rc.size_in_byte(),
                };
            }
            return arg_regs;
        }

        let mut offset = 0;
        for (c, s, rc, op) in vec![
            (mov8, 8, RegisterClassKind::GR64, MachineOpcode::MOVrm64),
            (mov4, 4, RegisterClassKind::GR32, MachineOpcode::MOVrm32),
        ]
        .into_iter()
        {
            for _ in 0..c {
                let r = RegisterOperand::new(self.func.regs.new_virt_reg(rc));
                let mem = if offset == 0 {
                    MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, fi.clone()))
                } else {
                    MachineOperand::Mem(MachineMemOperand::BaseFiOff(rbp, fi.clone(), offset))
                };
                let mov = MachineInst::new_simple(op, vec![mem], self.block_id).with_def(vec![r]);
                self.append_inst(mov);
                let mov = MachineInst::new_simple(
                    MachineOpcode::MOVmr64,
                    vec![
                        MachineOperand::Mem(MachineMemOperand::BaseOff(
                            RegisterOperand::new(self.func.regs.get_phys_reg(GR64::RSP)),
                            *off + offset as i32,
                        )),
                        MachineOperand::Register(r),
                    ],
                    self.block_id,
                );
                self.append_inst(mov);
                offset += s;
            }
        }
        *off += sz as i32;

        vec![]
    }

    fn convert_ret(&mut self, arg: NodeId) -> MachineInstId {
        let ret_ty = self
            .func
            .types
            .compound_ty(self.func.ty)
            .as_function()
            .ret_ty;

        if ret_ty != Type::Void {
            let arg = self.normal_arg(arg);
            let rc = match arg {
                MachineOperand::Register(RegisterOperand { id, sub_super }) => {
                    sub_super.unwrap_or_else(|| self.func.regs.arena_ref()[id].reg_class)
                }
                MachineOperand::Constant(MachineConstant::Int8(_)) => RegisterClassKind::GR8,
                MachineOperand::Constant(MachineConstant::Int32(_)) => RegisterClassKind::GR32,
                MachineOperand::Constant(MachineConstant::Int64(_)) => RegisterClassKind::GR64,
                MachineOperand::Constant(MachineConstant::F64(_)) => RegisterClassKind::XMM,
                _ => panic!(),
            };
            let opcode = mov_rx(rc, &arg).unwrap();
            let mov = MachineInst::new_simple(opcode, vec![arg], self.block_id).with_def(vec![
                RegisterOperand::new(self.func.regs.get_phys_reg(rc.return_value_register())),
            ]);
            self.append_inst(mov);
        }

        self.append_inst(MachineInst::new_simple(
            MachineOpcode::RET,
            vec![],
            self.block_id,
        ))
    }

    pub fn normal_arg(&mut self, arg: NodeId) -> MachineOperand {
        match &self.func.node_arena[arg] {
            Node::Operand(OperandNode::Imm(ImmediateKind::Int8(i))) => {
                MachineOperand::Constant(MachineConstant::Int8(*i))
            }
            Node::Operand(OperandNode::Imm(ImmediateKind::Int32(i))) => {
                MachineOperand::Constant(MachineConstant::Int32(*i))
            }
            Node::Operand(OperandNode::Imm(ImmediateKind::F64(f))) => {
                MachineOperand::Constant(MachineConstant::F64(*f))
            }
            Node::Operand(OperandNode::Slot(slot)) => MachineOperand::FrameIndex(*slot),
            Node::Operand(OperandNode::Mem(MemKind::Base(base))) => MachineOperand::Mem(
                MachineMemOperand::Base(*self.normal_arg(*base).as_register()),
            ),
            Node::Operand(OperandNode::Mem(MemKind::BaseOff(args))) => {
                MachineOperand::Mem(MachineMemOperand::BaseOff(
                    *self.normal_arg(args[0]).as_register(),
                    self.normal_arg(args[1]).as_constant().as_i32(),
                ))
            }
            Node::Operand(OperandNode::Mem(MemKind::BaseFi(args))) => {
                MachineOperand::Mem(MachineMemOperand::BaseFi(
                    *self.normal_arg(args[0]).as_register(),
                    *self.normal_arg(args[1]).as_frame_index(),
                ))
            }
            Node::Operand(OperandNode::Mem(MemKind::BaseAlignOff(args))) => {
                MachineOperand::Mem(MachineMemOperand::BaseAlignOff(
                    *self.normal_arg(args[0]).as_register(),
                    self.normal_arg(args[1]).as_constant().as_i32(),
                    *self.normal_arg(args[2]).as_register(),
                ))
            }
            Node::Operand(OperandNode::Mem(MemKind::BaseFiAlignOff(args))) => {
                MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(
                    *self.normal_arg(args[0]).as_register(),
                    *self.normal_arg(args[1]).as_frame_index(),
                    self.normal_arg(args[2]).as_constant().as_i32(),
                    *self.normal_arg(args[3]).as_register(),
                ))
            }
            Node::Operand(OperandNode::Mem(MemKind::BaseFiOff(args))) => {
                MachineOperand::Mem(MachineMemOperand::BaseFiOff(
                    *self.normal_arg(args[0]).as_register(),
                    *self.normal_arg(args[1]).as_frame_index(),
                    self.normal_arg(args[2]).as_constant().as_i32(),
                ))
            }
            Node::Operand(OperandNode::Mem(MemKind::BaseFiAlignOffOff(args))) => {
                MachineOperand::Mem(MachineMemOperand::BaseFiAlignOffOff(
                    *self.normal_arg(args[0]).as_register(),
                    *self.normal_arg(args[1]).as_frame_index(),
                    self.normal_arg(args[2]).as_constant().as_i32(),
                    *self.normal_arg(args[3]).as_register(),
                    self.normal_arg(args[4]).as_constant().as_i32(),
                ))
            }
            Node::Operand(OperandNode::Mem(MemKind::AddressAlignOff(args))) => {
                MachineOperand::Mem(MachineMemOperand::AddressAlignOff(
                    AddressKind::Global(
                        *self.normal_arg(args[0]).as_mem().as_address().as_global(),
                    ),
                    self.normal_arg(args[1]).as_constant().as_i32(),
                    *self.normal_arg(args[2]).as_register(),
                ))
            }
            Node::Operand(OperandNode::Mem(MemKind::AddressOff(args))) => {
                MachineOperand::Mem(MachineMemOperand::AddressOff(
                    AddressKind::Global(
                        *self.normal_arg(args[0]).as_mem().as_address().as_global(),
                    ),
                    self.normal_arg(args[1]).as_constant().as_i32(),
                ))
            }
            Node::Operand(OperandNode::Mem(MemKind::Address(arg))) => {
                MachineOperand::Mem(MachineMemOperand::Address(AddressKind::Constant(
                    *self.normal_arg(*arg).as_mem().as_address().as_const(),
                )))
            }
            Node::Operand(OperandNode::Addr(node::AddressKind::Const(id))) => {
                MachineOperand::Mem(MachineMemOperand::Address(AddressKind::Constant(*id)))
            }
            // MemNodeKind::AddressOff => MachineOperand::Mem(MachineMemOperand::AddressOff(
            //     inst::AddressKind::Global(*node.operand[0].as_address().as_global()),
            //     self.normal_operand(node.operand[1]).as_constant().as_i32(),
            // )),
            Node::Operand(OperandNode::Reg(r)) => {
                MachineOperand::Register(RegisterOperand::new(*r))
            }
            Node::Operand(OperandNode::Addr(node::AddressKind::FunctionName(name))) => {
                MachineOperand::Mem(MachineMemOperand::Address(AddressKind::FunctionName(
                    name.clone(),
                )))
            }
            Node::Operand(OperandNode::Addr(node::AddressKind::Global(id))) => {
                MachineOperand::Mem(MachineMemOperand::Address(AddressKind::Global(*id)))
            }
            Node::Operand(OperandNode::Block(id)) => MachineOperand::Branch(self.bb_map[id]),
            Node::IR(_) | Node::MI(_) => MachineOperand::Register(self.convert(arg).unwrap()),
            Node::None => MachineOperand::None,
            e => todo!("{:?}", e),
        }
    }
}

pub fn mov_rx(rc: RegisterClassKind, arg: &MachineOperand) -> Option<MachineOpcode> {
    let mov8rx = [
        MachineOpcode::MOVrr8,
        MachineOpcode::MOVri8,
        MachineOpcode::MOVrm8,
    ];
    let mov32rx = [
        MachineOpcode::MOVrr32,
        MachineOpcode::MOVri32,
        MachineOpcode::MOVrm32,
    ];
    let mov64rx = [
        MachineOpcode::MOVrr64,
        MachineOpcode::MOVri64,
        MachineOpcode::MOVrm64,
    ];
    let movsdrx = [
        MachineOpcode::MOVSDrr,
        MachineOpcode::MOVSDrm64,
        MachineOpcode::MOVSDrm,
    ];
    let idx = match arg {
        MachineOperand::Register(_) => 0,
        MachineOperand::Constant(_) => 1,
        MachineOperand::Mem(_) => 2,
        e => panic!("{:?}", e),
        // _ => return None,
    };
    match rc {
        RegisterClassKind::GR8 => Some(mov8rx[idx]),
        RegisterClassKind::GR32 => Some(mov32rx[idx]),
        RegisterClassKind::GR64 => Some(mov64rx[idx]),
        RegisterClassKind::XMM => Some(movsdrx[idx]),
    }
}

pub fn mov_mx(regs: &RegistersInfo, arg: &MachineOperand) -> Option<MachineOpcode> {
    // TODO: We'd better use another way to determine if arg is floating-point value or not
    if arg.get_type(regs).unwrap() == Type::f64 {
        return match arg {
            MachineOperand::Register(_) => Some(MachineOpcode::MOVSDmr),
            _ => None,
        };
    }

    let mov32mx = [MachineOpcode::MOVmr32, MachineOpcode::MOVmi32];
    let mov64mx = [MachineOpcode::MOVmr64, MachineOpcode::MOVmi64];
    // let mov64rx = [
    //     MachineOpcode::MOVrr64,
    //     MachineOpcode::MOVri64,
    //     MachineOpcode::MOVrm64,
    // ];
    let (bit, n) = match arg {
        MachineOperand::Register(r) => {
            if let Some(sub_super) = r.sub_super {
                (sub_super.size_in_bits(), 0)
            } else {
                (regs.arena_ref()[r.id].reg_class.size_in_bits(), 0)
            }
        }
        MachineOperand::Constant(c) => (c.size_in_bits(), 1),
        _ => return None, // TODO: Support Address?
    };
    match bit {
        32 => Some(mov32mx[n]),
        64 => Some(mov64mx[n]),
        _ => None,
    }
}
