use super::{super::frame_object::FrameIndexInfo, super::machine::instr::*, basic_block::*};
use crate::ir::{opcode::*, types::*};
use crate::util::allocator::*;
use id_arena::*;

pub type DAGNodeId = Id<DAGNode>;

#[derive(Debug, Clone)]
pub struct DAGNode {
    pub kind: NodeKind,
    pub operand: Vec<Raw<DAGNode>>,
    pub ty: Type,
    pub next: Option<Raw<DAGNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    // intermediate representation
    IR(IRNodeKind),
    // machine instruction
    MI(MINodeKind),
    // operand
    Operand(OperandNodeKind),
    None,
}

// x64 dependent
#[derive(Debug, Clone, PartialEq)]
pub enum MINodeKind {
    MOVrmi32,  // out = mov [rbp - fi.off + const_off]
    MOVrmri32, // out = mov [rbp - fi.off + off * align]
    MOVrrri32, // out = mov [base + off * align]
}

#[derive(Debug, Clone, PartialEq)]
pub enum OperandNodeKind {
    CondKind(CondKind),
    FrameIndex(FrameIndexInfo), // TODO
    Constant(ConstantKind),
    GlobalAddress(GlobalValueKind),
    BasicBlock(DAGBasicBlockId),
    Register(RegisterInfoRef),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRNodeKind {
    Entry,

    Load,
    Store,
    Add,
    Sub,
    Mul,
    Rem,
    Call,
    Phi,
    Setcc,
    BrCond,
    Brcc,
    Br,
    Ret,

    CopyToReg,
    CopyFromReg,
    StoreFiConstOff,
    StoreFiOff,
    StoreRegOff,

    CopyToLiveOut,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ConstantKind {
    Int32(i32),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CondKind {
    Eq,
    Le,
    Lt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GlobalValueKind {
    FunctionName(String),
}

impl Into<CondKind> for ICmpKind {
    fn into(self) -> CondKind {
        match self {
            ICmpKind::Eq => CondKind::Eq,
            ICmpKind::Le => CondKind::Le,
            ICmpKind::Lt => CondKind::Lt,
        }
    }
}

impl ConstantKind {
    pub fn add(self, n: ConstantKind) -> ConstantKind {
        match (self, n) {
            (ConstantKind::Int32(x), ConstantKind::Int32(y)) => ConstantKind::Int32(x + y),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            ConstantKind::Int32(_) => Type::Int32,
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            ConstantKind::Int32(0) => true,
            _ => false,
        }
    }
}

impl DAGNode {
    pub fn new(kind: NodeKind, operand: Vec<Raw<DAGNode>>, ty: Type) -> Self {
        Self {
            kind,
            ty,
            next: None,
            operand,
        }
    }

    pub fn set_next(mut self, next: Raw<DAGNode>) -> Self {
        self.next = Some(next);
        self
    }

    pub fn as_basic_block(&self) -> DAGBasicBlockId {
        match self.kind {
            NodeKind::Operand(OperandNodeKind::BasicBlock(bb)) => bb,
            _ => panic!(),
        }
    }

    pub fn as_cond_kind(&self) -> CondKind {
        match self.kind {
            NodeKind::Operand(OperandNodeKind::CondKind(ck)) => ck,
            _ => panic!(),
        }
    }

    pub fn as_constant(&self) -> ConstantKind {
        match self.kind {
            NodeKind::Operand(OperandNodeKind::Constant(c)) => c,
            _ => panic!(),
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self.kind, NodeKind::Operand(OperandNodeKind::Constant(_)))
    }

    pub fn is_frame_index(&self) -> bool {
        matches!(self.kind, NodeKind::Operand(OperandNodeKind::FrameIndex(_)))
    }

    pub fn is_operation(&self) -> bool {
        match self.kind {
            NodeKind::Operand(OperandNodeKind::CondKind(_))
            | NodeKind::Operand(OperandNodeKind::FrameIndex(_))
            | NodeKind::Operand(OperandNodeKind::Constant(_))
            | NodeKind::Operand(OperandNodeKind::GlobalAddress(_))
            | NodeKind::Operand(OperandNodeKind::BasicBlock(_))
            | NodeKind::None => false,
            _ => true,
        }
    }
}
