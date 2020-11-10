// use super::{node, node::*};
use crate::codegen::arch::dag::node::MemKind;
// use crate::codegen::arch::frame_object::FrameIndexInfo;
// use crate::codegen::arch::machine::abi::SystemV;
use crate::codegen::arch::machine::inst::*;
use crate::codegen::arch::machine::register::*;
// use crate::codegen::common::machine::calling_conv::{ArgumentRegisterOrder, CallingConv};
use crate::codegen::common::machine::inst::*;
use crate::codegen::common::machine::inst_def::DefOrUseReg;
pub use crate::codegen::common::new_dag::mc_convert::ScheduleContext;
use crate::codegen::common::new_dag::node::{
    IRNode, IROpcode, ImmediateKind, MINode, Node, NodeId, OperandNode,
};
use crate::codegen::common::types::MVType;
// use crate::ir::types::*;

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
                opcode: IROpcode::Div,
                args,
                mvty,
                ..
            }) => {
                let regs = match mvty {
                    MVType::i8 => to_phys!(GR32::EAX, GR32::EDX),
                    MVType::i32 => to_phys!(GR32::EAX, GR32::EDX),
                    _ => todo!(),
                };
                let (eax, edx) = (
                    RegisterOperand::new(self.func.regs.get_phys_reg(regs[0])),
                    RegisterOperand::new(self.func.regs.get_phys_reg(regs[0])),
                );
                let (mut lhs, mut rhs) = (self.normal_arg(args[0]), self.normal_arg(args[1]));
                if mvty == &MVType::i8 {
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
                    vec![MachineOperand::Register(eax)],
                    Some(regs[0].reg_class()),
                    self.block_id,
                );
                self.append_inst(copy)
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

    fn convert_ret(&mut self, arg: NodeId) -> MachineInstId {
        let arg = self.normal_arg(arg);
        let ret_ty = self
            .func
            .types
            .compound_ty(self.func.ty)
            .as_function()
            .ret_ty;
        let rc = ty2rc(&ret_ty).unwrap();
        let opcode = mov_rx(rc, &arg).unwrap();
        let mov = MachineInst::new_simple(opcode, vec![arg], self.block_id).with_def(vec![
            RegisterOperand::new(self.func.regs.get_phys_reg(rc.return_value_register())),
        ]);
        self.append_inst(mov);

        self.append_inst(MachineInst::new_simple(
            MachineOpcode::RET,
            vec![],
            self.block_id,
        ))
    }

    pub fn normal_arg(&mut self, arg: NodeId) -> MachineOperand {
        match &self.func.node_arena[arg] {
            Node::Operand(OperandNode::Imm(ImmediateKind::Int32(i))) => {
                MachineOperand::Constant(MachineConstant::Int32(*i))
            }
            Node::Operand(OperandNode::Slot(slot)) => MachineOperand::FrameIndex(*slot),
            Node::Operand(OperandNode::Mem(MemKind::BaseFi(base, slot))) => {
                MachineOperand::Mem(MachineMemOperand::BaseFi(
                    *self.normal_arg(*base).as_register(),
                    *self.normal_arg(*slot).as_frame_index(),
                ))
            }
            Node::Operand(OperandNode::Reg(r)) => {
                MachineOperand::Register(RegisterOperand::new(*r))
            }
            Node::IR(_) | Node::MI(_) => MachineOperand::Register(self.convert(arg).unwrap()),
            e => todo!("{:?}", e),
        }
    }
}

pub fn mov_rx(rc: RegisterClassKind, arg: &MachineOperand) -> Option<MachineOpcode> {
    let mov8rx = [MachineOpcode::MOVrr8, MachineOpcode::MOVri8];
    let mov32rx = [MachineOpcode::MOVrr32, MachineOpcode::MOVri32];
    let mov64rx = [MachineOpcode::MOVrr64, MachineOpcode::MOVri64];
    let movsdrx = [MachineOpcode::MOVSDrr, MachineOpcode::MOVSDrm64];
    let idx = match arg {
        MachineOperand::Register(_) => 0,
        MachineOperand::Constant(_) => 1,
        _ => return None,
    };
    match rc {
        RegisterClassKind::GR8 => Some(mov8rx[idx]),
        RegisterClassKind::GR32 => Some(mov32rx[idx]),
        RegisterClassKind::GR64 => Some(mov64rx[idx]),
        RegisterClassKind::XMM => Some(movsdrx[idx]),
    }
}
