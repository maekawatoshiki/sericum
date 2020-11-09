use crate::codegen::{
    arch::{
        dag::node::MemNodeKind,
        frame_object::FrameIndexInfo,
        machine::{inst::MachineOpcode, register::*},
    },
    common::{new_dag::basic_block::DAGBasicBlockId, types::MVType},
};
use crate::ir::{constant_pool, global_val::GlobalVariableId, opcode::*, types::*};
use id_arena::*;
// use std::fmt;

//////////

pub type NodeId = Id<Node>;

pub type IROpcode = IRNodeKind;

#[derive(Debug, Clone)]
pub enum Node {
    IR(IRNode),
    MI(MINode),
    Operand(OperandNode),
    None,
}

#[derive(Debug, Clone)]
pub struct IRNode {
    pub opcode: IROpcode,
    pub args: Vec<NodeId>,
    pub ty: Type,
    pub mvty: MVType,
    pub next: Option<NodeId>,
    pub chain: Option<NodeId>,
}

#[derive(Debug, Clone)]
pub struct MINode {
    pub opcode: MachineOpcode,
    pub args: Vec<NodeId>,
    pub next: Option<NodeId>,
    pub chain: Option<NodeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OperandNode {
    Imm(ImmediateKind),
    Reg(RegisterId),
    Addr(AddressKind),
    Slot(FrameIndexInfo), // TODO: FrameIndex will be named Slot
    Block(DAGBasicBlockId),
    Mem(MemNodeKind),
    CC(CondKind),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ImmediateKind {
    Int8(i8),
    Int32(i32),
    Int64(i64),
    F64(f64),
}

impl Node {
    pub fn as_ir(&self) -> &IRNode {
        match self {
            Node::IR(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_ir_mut(&mut self) -> &mut IRNode {
        match self {
            Node::IR(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_operand(&self) -> &OperandNode {
        match self {
            Node::Operand(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_i32(&self) -> i32 {
        self.as_operand().as_imm().as_i32()
    }

    pub fn args_mut(&mut self) -> &mut [NodeId] {
        match self {
            Self::IR(ir) => &mut ir.args,
            Self::MI(mi) => &mut mi.args,
            Self::Operand(_) => &mut [],
            Self::None => &mut [],
        }
    }
}

impl IRNode {
    pub fn new(opcode: IROpcode) -> Self {
        Self {
            opcode,
            args: vec![],
            ty: Type::Void,
            mvty: MVType::Void,
            next: None,
            chain: None,
        }
    }

    pub fn args(mut self, args: Vec<NodeId>) -> Self {
        self.args = args;
        self
    }

    pub fn ty(mut self, ty: Type) -> Self {
        self.ty = ty;
        self.mvty = ty.into();
        self
    }
}

impl OperandNode {
    pub fn i32(i: i32) -> Self {
        Self::Imm(ImmediateKind::Int32(i))
    }

    pub fn imm(imm: ImmediateKind) -> Self {
        Self::Imm(imm)
    }

    pub fn slot(slot: FrameIndexInfo) -> Self {
        Self::Slot(slot)
    }

    pub fn as_imm(&self) -> &ImmediateKind {
        match self {
            Self::Imm(x) => x,
            _ => panic!(),
        }
    }
}

impl ImmediateKind {
    pub fn as_i32(&self) -> i32 {
        match self {
            Self::Int32(x) => *x,
            _ => panic!(),
        }
    }
}

impl Into<Node> for i32 {
    fn into(self) -> Node {
        Node::Operand(OperandNode::Imm(ImmediateKind::Int32(self)))
    }
}

impl Into<Node> for IRNode {
    fn into(self) -> Node {
        Node::IR(self)
    }
}

impl Into<Node> for OperandNode {
    fn into(self) -> Node {
        Node::Operand(self)
    }
}

impl Into<Node> for ImmediateKind {
    fn into(self) -> Node {
        Node::Operand(OperandNode::Imm(self))
    }
}

impl Into<Node> for AddressKind {
    fn into(self) -> Node {
        Node::Operand(OperandNode::Addr(self))
    }
}

impl Into<Node> for DAGBasicBlockId {
    fn into(self) -> Node {
        Node::Operand(OperandNode::Block(self))
    }
}

impl Into<Node> for FrameIndexInfo {
    fn into(self) -> Node {
        Node::Operand(OperandNode::slot(self))
    }
}

impl Into<Node> for ICmpKind {
    fn into(self) -> Node {
        Node::Operand(OperandNode::CC(self.into()))
    }
}

impl Into<Node> for FCmpKind {
    fn into(self) -> Node {
        Node::Operand(OperandNode::CC(self.into()))
    }
}

impl Into<Node> for RegisterId {
    fn into(self) -> Node {
        Node::Operand(OperandNode::Reg(self))
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum IRNodeKind {
    Entry,
    Root,

    Load,
    Store,
    Shl,
    AShr,
    LShr,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    SIToFP,
    FPToSI,
    Call,
    Phi,
    Setcc,
    BrCond,
    Brcc,
    FPBrcc,
    Br,
    Ret,
    Sext,
    Bitcast,
    FCmp,

    FIAddr,
    GlobalAddr,
    ConstAddr,

    CopyToReg,
    CopyFromReg,
    StoreFiConstOff,
    StoreFiOff,
    StoreRegOff,

    CopyToLiveOut,
    RegClass,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CondKind {
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
    UEq,
    UNe,
    ULe,
    ULt,
    UGe,
    UGt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AddressKind {
    FunctionName(String),
    Global(GlobalVariableId),
    Const(constant_pool::ConstantId),
}

impl Into<CondKind> for ICmpKind {
    fn into(self) -> CondKind {
        match self {
            ICmpKind::Eq => CondKind::Eq,
            ICmpKind::Ne => CondKind::Ne,
            ICmpKind::Le => CondKind::Le,
            ICmpKind::Lt => CondKind::Lt,
            ICmpKind::Ge => CondKind::Ge,
            ICmpKind::Gt => CondKind::Gt,
        }
    }
}

impl Into<CondKind> for FCmpKind {
    fn into(self) -> CondKind {
        match self {
            FCmpKind::UEq => CondKind::UEq,
            FCmpKind::UNe => CondKind::UNe,
            FCmpKind::ULe => CondKind::ULe,
            FCmpKind::ULt => CondKind::ULt,
            FCmpKind::UGe => CondKind::UGe,
            FCmpKind::UGt => CondKind::UGt,
        }
    }
}

impl CondKind {
    pub fn flip(self) -> CondKind {
        match self {
            Self::Le => Self::Ge,
            Self::Lt => Self::Gt,
            Self::ULe => Self::UGe,
            Self::ULt => Self::UGt,
            e => e,
        }
    }
}
