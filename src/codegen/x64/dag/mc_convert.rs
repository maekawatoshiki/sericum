// TODO: dirty code

use super::super::machine::{inst, inst::*, register::*};
use super::{node, node::*};
use crate::codegen::common::{
    dag::{basic_block::*, function::*, module::*},
    machine::{basic_block::*, function::*, inst_def::DefOrUseReg, module::*},
};
use crate::codegen::x64::frame_object::FrameIndexInfo;
use crate::ir::types::*;
use crate::util::allocator::*;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

pub struct ScheduleByBlock<'a> {
    types: &'a Types,
    cur_func: &'a DAGFunction,
    inst_arena: &'a mut InstructionArena,
    node2reg: FxHashMap<Raw<DAGNode>, Option<RegisterId>>,
    cur_bb: MachineBasicBlockId,
    iseq: &'a mut Vec<MachineInstId>,
    bb_map: &'a FxHashMap<DAGBasicBlockId, MachineBasicBlockId>,
    node2inst: &'a mut FxHashMap<Raw<DAGNode>, MachineInstId>,
}

pub fn convert_module(module: DAGModule) -> MachineModule {
    let mut functions = Arena::new();
    for (_, func) in module.functions {
        functions.alloc(convert_function(&module.types, func));
    }
    MachineModule::new(module.name, functions, module.types, module.global_vars)
}

pub fn convert_function(types: &Types, dag_func: DAGFunction) -> MachineFunction {
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

    let mut inst_arena = InstructionArena::new();
    let mut node2inst = FxHashMap::default();

    for dag_bb_id in &dag_func.dag_basic_blocks {
        let node = &dag_func.dag_basic_block_arena[*dag_bb_id];
        let bb_id = *bb_map.get(dag_bb_id).unwrap();
        let mut iseq = vec![];

        let entry = match node.entry {
            Some(entry) => entry,
            None => continue,
        };

        ScheduleByBlock {
            types,
            cur_func: &dag_func,
            inst_arena: &mut inst_arena,
            node2reg: FxHashMap::default(),
            cur_bb: bb_id,
            iseq: &mut iseq,
            bb_map: &bb_map,
            node2inst: &mut node2inst,
        }
        .convert(entry);

        mbbs.arena[bb_id].iseq = RefCell::new(iseq);
    }

    MachineFunction::new(dag_func, mbbs, inst_arena)
}

impl<'a> ScheduleByBlock<'a> {
    pub fn convert(&mut self, node: Raw<DAGNode>) -> Option<RegisterId> {
        if let Some(reg_id) = self.node2reg.get(&node) {
            return *reg_id;
        }

        let reg_id = match node.kind {
            NodeKind::IR(IRNodeKind::Entry) => None,
            _ => {
                let inst_id = self.convert_node_to_inst(node);
                self.inst_arena[inst_id].get_def_reg()
            }
        };
        self.node2reg.insert(node, reg_id);

        if let Some(next) = node.next {
            self.convert(next);
        }

        reg_id
    }

    fn convert_node_to_inst(&mut self, node: Raw<DAGNode>) -> MachineInstId {
        if let Some(inst_id) = self.node2inst.get(&node) {
            return *inst_id;
        }

        #[rustfmt::skip]
        macro_rules! cond_kind {($id:expr)=>{ $id.as_cond_kind() };}

        let inst_id = match &node.kind {
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
                self.append_inst(inst)
            }
            NodeKind::IR(IRNodeKind::CopyToReg) => {
                let val = self.normal_operand(node.operand[1]);
                let dst = match &node.operand[0].kind {
                    NodeKind::Operand(OperandNodeKind::Register(r)) => *r,
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
            NodeKind::IR(IRNodeKind::Div) => {
                let eax = self.cur_func.regs_info.get_phys_reg(GR32::EAX);
                let edx = self.cur_func.regs_info.get_phys_reg(GR32::EDX);

                let op1 = self.normal_operand(node.operand[0]);
                let op2 = self.normal_operand(node.operand[1]);

                self.append_inst(
                    MachineInst::new_simple(mov_n_rx(32, &op1).unwrap(), vec![op1], self.cur_bb)
                        .with_def(vec![eax]),
                );

                self.append_inst(
                    MachineInst::new_simple(MachineOpcode::CDQ, vec![], self.cur_bb)
                        .with_imp_defs(vec![eax, edx])
                        .with_imp_use(eax),
                );

                assert_eq!(op2.get_type(&self.cur_func.regs_info), Some(Type::i32));
                let inst1 = MachineInst::new(
                    &self.cur_func.regs_info,
                    mov_n_rx(32, &op2).unwrap(),
                    vec![op2],
                    Some(RegisterClassKind::GR32), // TODO: support other types
                    self.cur_bb,
                );
                let op2 = MachineOperand::Register(inst1.def[0]);
                self.append_inst(inst1);

                self.append_inst(
                    MachineInst::new_simple(MachineOpcode::IDIV, vec![op2], self.cur_bb)
                        .with_imp_defs(vec![eax, edx])
                        .with_imp_uses(vec![eax, edx]),
                );

                let copy_inst = MachineInst::new(
                    &self.cur_func.regs_info,
                    MachineOpcode::Copy,
                    vec![MachineOperand::Register(eax)],
                    Some(RegisterClassKind::GR32), // TODO
                    self.cur_bb,
                );
                self.append_inst(copy_inst)
            }
            NodeKind::IR(IRNodeKind::Rem) => {
                let eax = self.cur_func.regs_info.get_phys_reg(GR32::EAX);
                let edx = self.cur_func.regs_info.get_phys_reg(GR32::EDX);

                let op1 = self.normal_operand(node.operand[0]);
                let op2 = self.normal_operand(node.operand[1]);

                self.append_inst(
                    MachineInst::new_simple(mov_n_rx(32, &op1).unwrap(), vec![op1], self.cur_bb)
                        .with_def(vec![eax]),
                );

                self.append_inst(
                    MachineInst::new_simple(MachineOpcode::CDQ, vec![], self.cur_bb)
                        .with_imp_defs(vec![eax, edx])
                        .with_imp_use(eax),
                );

                assert_eq!(op2.get_type(&self.cur_func.regs_info), Some(Type::i32));
                let inst1 = MachineInst::new(
                    &self.cur_func.regs_info,
                    mov_n_rx(32, &op2).unwrap(),
                    vec![op2],
                    Some(RegisterClassKind::GR32), // TODO: support other types
                    self.cur_bb,
                );
                let op2 = MachineOperand::Register(inst1.def[0]);
                self.append_inst(inst1);

                self.append_inst(
                    MachineInst::new_simple(MachineOpcode::IDIV, vec![op2], self.cur_bb)
                        .with_imp_defs(vec![eax, edx])
                        .with_imp_uses(vec![eax, edx]),
                );

                self.append_inst(MachineInst::new(
                    &self.cur_func.regs_info,
                    MachineOpcode::Copy,
                    vec![MachineOperand::Register(edx)],
                    Some(RegisterClassKind::GR32), // TODO
                    self.cur_bb,
                ))
            }
            NodeKind::IR(IRNodeKind::Setcc) => {
                let new_op1 = self.normal_operand(node.operand[1]);
                let new_op2 = self.normal_operand(node.operand[2]);
                let inst = MachineInst::new(
                    &self.cur_func.regs_info,
                    match cond_kind!(node.operand[0]) {
                        CondKind::Eq => MachineOpcode::Seteq,
                        CondKind::Le => MachineOpcode::Setle,
                        CondKind::Lt => MachineOpcode::Setlt,
                        _ => unimplemented!(),
                    },
                    vec![new_op1, new_op2],
                    ty2rc(&node.ty),
                    self.cur_bb,
                );
                self.append_inst(inst)
            }
            NodeKind::IR(IRNodeKind::Brcc) => {
                let op0 = self.normal_operand(node.operand[1]);
                let op1 = self.normal_operand(node.operand[2]);

                self.append_inst(MachineInst::new_simple(
                    if op0.is_register() && op1.is_constant() {
                        MachineOpcode::CMPri
                    } else if op0.is_register() && op1.is_register() {
                        MachineOpcode::CMPrr
                    } else {
                        unreachable!()
                    },
                    vec![op0, op1],
                    self.cur_bb,
                ));

                self.append_inst(MachineInst::new_simple(
                    match cond_kind!(node.operand[0]) {
                        CondKind::Eq => MachineOpcode::JE,
                        CondKind::Le => MachineOpcode::JLE,
                        CondKind::Lt => MachineOpcode::JL,
                        CondKind::Ge => MachineOpcode::JGE,
                        CondKind::Gt => MachineOpcode::JG,
                        _ => unreachable!(),
                    },
                    vec![MachineOperand::Branch(
                        self.get_machine_bb(node.operand[3].as_basic_block()),
                    )],
                    self.cur_bb,
                ))
            }
            NodeKind::IR(IRNodeKind::FPBrcc) => {
                let op0 = self.normal_operand(node.operand[1]);
                let op1 = self.normal_operand(node.operand[2]);

                self.append_inst(MachineInst::new_simple(
                    MachineOpcode::UCOMISDrr,
                    vec![op0, op1],
                    self.cur_bb,
                ));

                self.append_inst(MachineInst::new_simple(
                    match cond_kind!(node.operand[0]) {
                        CondKind::UEq => MachineOpcode::JE,
                        CondKind::ULe => MachineOpcode::JBE,
                        CondKind::ULt => MachineOpcode::JB,
                        CondKind::UGe => MachineOpcode::JAE,
                        CondKind::UGt => MachineOpcode::JA,
                        _ => unreachable!(),
                    },
                    vec![MachineOperand::Branch(
                        self.get_machine_bb(node.operand[3].as_basic_block()),
                    )],
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
            let ret_reg = ty2rc(&ty).unwrap().return_value_register();
            let set_ret_val = MachineInst::new_simple(
                mov_rx(self.types, &self.cur_func.regs_info, &val).unwrap(),
                vec![val],
                self.cur_bb,
            )
            .with_def(vec![self.cur_func.regs_info.get_phys_reg(ret_reg)]);
            self.append_inst(set_ret_val);
        }

        self.append_inst(MachineInst::new_simple(
            MachineOpcode::RET,
            vec![],
            self.cur_bb,
        ))
    }

    fn move2reg(&self, r: RegisterId, src: MachineOperand) -> MachineInst {
        let opcode = mov_rx(self.types, &self.cur_func.regs_info, &src).unwrap();
        MachineInst::new_simple(opcode, vec![src], self.cur_bb).with_def(vec![r])
    }

    fn convert_call_dag(&mut self, node: &DAGNode) -> MachineInstId {
        let mut arg_regs = vec![self.cur_func.regs_info.get_phys_reg(GR64::RSP)]; // call uses RSP
        let mut off = 0i32;

        // println!("T {:?}", self.types.to_string(node.operand[0].ty));
        let f_ty = node.operand[0].ty;

        let mut args = vec![];
        for (i, operand) in node.operand[1..].iter().enumerate() {
            let byval = self
                .types
                .base
                .borrow()
                .as_function_ty(f_ty)
                .unwrap()
                .params_attr
                .get(&i)
                .map_or(false, |attr| attr.byval);
            if byval {
                args.push(MachineOperand::None);
            } else {
                args.push(self.normal_operand(*operand));
            }
        }

        let mut arg_regs_order = GeneralArgRegOrder::new();

        for (i, arg) in args.into_iter().enumerate() {
            let (ty, byval) = {
                let base = self.types.base.borrow();
                let f = &base.as_function_ty(f_ty).unwrap();
                (
                    *f.params_ty.get(i).unwrap(),
                    f.params_attr.get(&i).map_or(false, |attr| attr.byval),
                )
            };

            if byval {
                // TODO
                let lea = &node.operand[1 + i];
                let mem = lea.operand[0];
                let fi = match self.normal_operand(mem) {
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
            };

            let reg_class = ty2rc(&ty).unwrap();
            let inst = match arg_regs_order.next(reg_class) {
                Some(arg_reg) => {
                    let r = self.cur_func.regs_info.get_phys_reg(arg_reg);
                    arg_regs.push(r.clone());
                    self.move2reg(r, arg)
                }
                None => {
                    // Put the exceeded value onto the stack
                    let inst = MachineInst::new_simple(
                        mov_mx(&self.cur_func.regs_info, &arg).unwrap(),
                        vec![
                            MachineOperand::Mem(MachineMemOperand::BaseOff(
                                self.cur_func.regs_info.get_phys_reg(GR64::RSP),
                                off,
                            )),
                            arg,
                        ],
                        self.cur_bb,
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
                self.cur_bb,
            )
            .with_imp_def(self.cur_func.regs_info.get_phys_reg(GR64::RSP))
            .with_imp_use(self.cur_func.regs_info.get_phys_reg(GR64::RSP)),
        );

        let callee = self.normal_operand(node.operand[0]);
        let ret_reg = self.cur_func.regs_info.get_phys_reg(
            ty2rc(&node.ty)
                .unwrap_or(RegisterClassKind::GR32)
                .return_value_register(),
        );
        let call_inst = self.append_inst(
            MachineInst::new_simple(MachineOpcode::CALL, vec![callee], self.cur_bb)
                .with_imp_uses(arg_regs)
                .with_imp_defs({
                    let mut defs = vec![self.cur_func.regs_info.get_phys_reg(GR64::RSP)];
                    if node.ty != Type::Void {
                        defs.push(ret_reg)
                    }
                    defs
                }),
        );

        self.append_inst(
            MachineInst::new_simple(
                MachineOpcode::AdjStackUp,
                vec![MachineOperand::imm_i32(off)],
                self.cur_bb,
            )
            .with_imp_def(self.cur_func.regs_info.get_phys_reg(GR64::RSP))
            .with_imp_use(self.cur_func.regs_info.get_phys_reg(GR64::RSP)),
        );

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
        self.append_inst(copy)
    }

    fn pass_struct_byval(
        &mut self,
        arg_regs_order: &mut GeneralArgRegOrder,
        off: &mut i32,
        ty: Type,
        fi: FrameIndexInfo,
    ) -> Vec<RegisterId> {
        let mut arg_regs = vec![];
        let struct_ty = self.cur_func.types.get_element_ty(ty, None).unwrap();
        let base = &self.cur_func.types.base.borrow();
        let struct_ty = base.as_struct_ty(struct_ty).unwrap();
        let sz = struct_ty.size();
        let mov8 = sz / 8;
        let mov4 = (sz - 8 * mov8) / 4;
        let rbp = self.cur_func.regs_info.get_phys_reg(GR64::RBP);
        assert!((sz - 8 * mov8) % 4 == 0);

        if sz <= 16 {
            let mut off = 0;
            for (c, s, rc, op) in vec![
                (mov8, 8, RegisterClassKind::GR64, MachineOpcode::MOVrm64),
                (mov4, 4, RegisterClassKind::GR32, MachineOpcode::MOVrm32),
            ]
            .into_iter()
            {
                for _ in 0..c {
                    let xmm = struct_ty.get_type_at(off) == Some(&Type::f64);
                    let r = self.cur_func.regs_info.get_phys_reg(
                        arg_regs_order
                            .next(if xmm { RegisterClassKind::XMM } else { rc })
                            .unwrap(),
                    );
                    arg_regs.push(r);
                    let mem = if off == 0 {
                        MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, fi.clone()))
                    } else {
                        MachineOperand::Mem(MachineMemOperand::BaseFiOff(
                            rbp,
                            fi.clone(),
                            off as i32,
                        ))
                    };
                    off += s;
                    let mov = MachineInst::new_simple(
                        if xmm { MachineOpcode::MOVSDrm } else { op },
                        vec![mem],
                        self.cur_bb,
                    )
                    .with_def(vec![r]);
                    self.append_inst(mov);
                }
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
                let r = self.cur_func.regs_info.new_virt_reg(rc);
                let mem = if offset == 0 {
                    MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, fi.clone()))
                } else {
                    MachineOperand::Mem(MachineMemOperand::BaseFiOff(rbp, fi.clone(), offset))
                };
                let mov = MachineInst::new_simple(op, vec![mem], self.cur_bb).with_def(vec![r]);
                self.append_inst(mov);
                let mov = MachineInst::new_simple(
                    MachineOpcode::MOVmr64,
                    vec![
                        MachineOperand::Mem(MachineMemOperand::BaseOff(
                            self.cur_func.regs_info.get_phys_reg(GR64::RSP),
                            *off + offset as i32,
                        )),
                        MachineOperand::Register(r),
                    ],
                    self.cur_bb,
                );
                self.append_inst(mov);
                offset += s;
            }
        }
        *off += sz as i32;

        vec![]
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
                _ => unreachable!()
                // node::AddressKind::GlobalName(n) => MachineOperand::Mem(
                //     MachineMemOperand::Address(inst::AddressKind::GlobalName(n.clone())),
                // ),
            },
            NodeKind::Operand(OperandNodeKind::BasicBlock(id)) => {
                MachineOperand::Branch(self.get_machine_bb(id))
            }
            NodeKind::Operand(OperandNodeKind::Register(ref r)) => MachineOperand::Register(*r),
            NodeKind::Operand(OperandNodeKind::Mem(ref mem)) => match mem {
                MemNodeKind::Base => MachineOperand::Mem(MachineMemOperand::Base(
                    *self.normal_operand(node.operand[0]).as_register(),
                )),
                MemNodeKind::BaseAlignOff => MachineOperand::Mem(MachineMemOperand::BaseAlignOff(
                    *self.normal_operand(node.operand[0]).as_register(),
                    self.normal_operand(node.operand[1]).as_constant().as_i32(),
                    *self.normal_operand(node.operand[2]).as_register(),
                )),
                MemNodeKind::BaseFi => MachineOperand::Mem(MachineMemOperand::BaseFi(
                    *self.normal_operand(node.operand[0]).as_register(),
                    *self.normal_operand(node.operand[1]).as_frame_index(),
                )),
                MemNodeKind::BaseFiAlignOff => {
                    MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(
                        *self.normal_operand(node.operand[0]).as_register(),
                        *self.normal_operand(node.operand[1]).as_frame_index(),
                        self.normal_operand(node.operand[2]).as_constant().as_i32(),
                        *self.normal_operand(node.operand[3]).as_register(),
                    ))
                }
                MemNodeKind::BaseFiOff => MachineOperand::Mem(MachineMemOperand::BaseFiOff(
                    *self.normal_operand(node.operand[0]).as_register(),
                    *self.normal_operand(node.operand[1]).as_frame_index(),
                    self.normal_operand(node.operand[2]).as_constant().as_i32(),
                )),
                MemNodeKind::AddressOff => MachineOperand::Mem(MachineMemOperand::AddressOff(
                    inst::AddressKind::Global(*node.operand[0].as_address().as_global()),
                    self.normal_operand(node.operand[1]).as_constant().as_i32(),
                )),
                MemNodeKind::AddressAlignOff => {
                    MachineOperand::Mem(MachineMemOperand::AddressAlignOff(
                        inst::AddressKind::Global(*node.operand[0].as_address().as_global()),
                        self.normal_operand(node.operand[1]).as_constant().as_i32(),
                        *self.normal_operand(node.operand[2]).as_register(),
                    ))
                }
                MemNodeKind::Address => MachineOperand::Mem(MachineMemOperand::Address(match node
                    .operand[0]
                    .as_address()
                {
                    node::AddressKind::Global(id) => inst::AddressKind::Global(*id),
                    node::AddressKind::FunctionName(name) => {
                        inst::AddressKind::FunctionName(name.clone())
                    }
                })),
            },
            NodeKind::None => MachineOperand::None,
            _ => MachineOperand::Register(self.convert(node).unwrap()),
        }
    }

    fn get_machine_bb(&self, dag_bb_id: DAGBasicBlockId) -> MachineBasicBlockId {
        *self.bb_map.get(&dag_bb_id).unwrap()
    }

    pub fn append_inst(&mut self, inst: MachineInst) -> MachineInstId {
        let inst_id = self.inst_arena.alloc(&self.cur_func.regs_info, inst);
        self.iseq.push(inst_id);
        inst_id
    }
}

pub fn mov_n_rx(bit: usize, x: &MachineOperand) -> Option<MachineOpcode> {
    // TODO: refine code
    assert!(bit > 0 && ((bit & (bit - 1)) == 0));

    let mov32rx = [MachineOpcode::MOVrr32, MachineOpcode::MOVri32];
    let xidx = match x {
        MachineOperand::Register(_) => 0,
        MachineOperand::Constant(_) => 1,
        _ => return None, // TODO: Support Address?
    };
    match bit {
        32 => Some(mov32rx[xidx]),
        _ => None,
    }
}

// TODO: Will be deprecated
pub fn mov_rx(tys: &Types, regs_info: &RegistersInfo, x: &MachineOperand) -> Option<MachineOpcode> {
    // TODO: special handling for float
    if x.get_type(regs_info).unwrap() == Type::f64 {
        return match x {
            MachineOperand::Constant(_) => Some(MachineOpcode::MOVSDrm64),
            MachineOperand::FrameIndex(_) | MachineOperand::Mem(_) => Some(MachineOpcode::MOVSDrm),
            MachineOperand::Register(_) => Some(MachineOpcode::MOVSDrr),
            _ => None,
        };
    }

    let mov8rx = [MachineOpcode::MOVrr8];
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
    let (bit, xidx) = match x {
        MachineOperand::Register(r) => (regs_info.arena_ref()[*r].reg_class.size_in_bits(), 0),
        MachineOperand::Constant(c) => (c.size_in_bits(), 1),
        MachineOperand::Mem(m) => (m.get_type().unwrap().size_in_bits(tys), 2),
        _ => return None, // TODO: Support Address?
    };
    match bit {
        8 => Some(mov8rx[xidx]),
        32 => Some(mov32rx[xidx]),
        64 => Some(mov64rx[xidx]),
        _ => None,
    }
}

pub fn mov_mx(regs_info: &RegistersInfo, x: &MachineOperand) -> Option<MachineOpcode> {
    if x.get_type(regs_info).unwrap() == Type::f64 {
        return match x {
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
    let (bit, n) = match x {
        MachineOperand::Register(r) => (regs_info.arena_ref()[*r].reg_class.size_in_bits(), 0),
        MachineOperand::Constant(c) => (c.size_in_bits(), 1),
        _ => return None, // TODO: Support Address?
    };
    match bit {
        32 => Some(mov32mx[n]),
        64 => Some(mov64mx[n]),
        _ => None,
    }
}
