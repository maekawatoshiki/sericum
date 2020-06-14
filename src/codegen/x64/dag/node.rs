use super::{
    super::frame_object::FrameIndexInfo,
    super::machine::{inst::MachineOpcode, register::*},
};
use crate::codegen::common::dag::basic_block::*;
use crate::ir::{opcode::*, types::*};
use crate::util::allocator::*;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::fmt;

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

pub type MINodeKind = MachineOpcode;

#[derive(Debug, Clone, PartialEq)]
pub enum OperandNodeKind {
    CondKind(CondKind),
    FrameIndex(FrameIndexInfo), // TODO
    Constant(ConstantKind),
    Address(AddressKind),
    BasicBlock(DAGBasicBlockId),
    Register(RegisterId),
    Mem(MemNodeKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRNodeKind {
    Entry,

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
    Call,
    Phi,
    Setcc,
    BrCond,
    Brcc,
    FPBrcc,
    Br,
    Ret,
    Sext,
    FCmp,

    FIAddr,

    CopyToReg,
    CopyFromReg,
    StoreFiConstOff,
    StoreFiOff,
    StoreRegOff,

    CopyToLiveOut,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ConstantKind {
    Int8(i8),
    Int32(i32),
    Int64(i64),
    F64(f64),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CondKind {
    Eq,
    Le,
    Lt,
    Ge,
    Gt,
    UEq,
    ULe,
    ULt,
    UGe,
    UGt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AddressKind {
    FunctionName(String),
}

// TODO: target dependent
#[derive(Debug, Clone, PartialEq)]
pub enum MemNodeKind {
    BaseFi,
    BaseFiOff,
    BaseFiAlignOff,
    BaseAlignOff,
    Base,
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

impl Into<CondKind> for FCmpKind {
    fn into(self) -> CondKind {
        match self {
            FCmpKind::UEq => CondKind::UEq,
            FCmpKind::ULe => CondKind::ULe,
            FCmpKind::ULt => CondKind::ULt,
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

impl ConstantKind {
    pub fn add(self, n: ConstantKind) -> ConstantKind {
        match (self, n) {
            (ConstantKind::Int32(x), ConstantKind::Int32(y)) => ConstantKind::Int32(x + y),
            (ConstantKind::Int64(x), ConstantKind::Int64(y)) => ConstantKind::Int64(x + y),
            (ConstantKind::F64(x), ConstantKind::F64(y)) => ConstantKind::F64(x + y),
            _ => unimplemented!(),
        }
    }

    pub fn is_power_of_two(&self) -> Option<u32> {
        match self {
            ConstantKind::Int32(x) if (*x as usize).is_power_of_two() => Some(x.trailing_zeros()),
            ConstantKind::Int64(x) if (*x as usize).is_power_of_two() => Some(x.trailing_zeros()),
            _ => None,
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            ConstantKind::Int8(_) => Type::Int8,
            ConstantKind::Int32(_) => Type::Int32,
            ConstantKind::Int64(_) => Type::Int64,
            ConstantKind::F64(_) => Type::F64,
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            ConstantKind::Int8(0) | ConstantKind::Int32(0) | ConstantKind::Int64(0) => true,
            ConstantKind::F64(f) if *f == 0.0 => true,
            _ => false,
        }
    }

    pub fn as_i32(&self) -> i32 {
        match self {
            Self::Int32(i) => *i,
            _ => panic!(),
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

    pub fn new_none() -> Self {
        Self {
            kind: NodeKind::None,
            ty: Type::Void,
            next: None,
            operand: vec![],
        }
    }

    pub fn new_phys_reg<T: TargetRegisterTrait>(regs_info: &RegistersInfo, reg: T) -> Self {
        Self {
            kind: NodeKind::Operand(OperandNodeKind::Register(regs_info.get_phys_reg(reg))),
            ty: rc2ty(reg.as_phys_reg().reg_class()),
            next: None,
            operand: vec![],
        }
    }

    pub fn new_mem(mem: MemNodeKind, operands: Vec<Raw<DAGNode>>) -> Self {
        Self::new(
            NodeKind::Operand(OperandNodeKind::Mem(mem)),
            operands,
            Type::Void,
        )
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

    pub fn as_frame_index(&self) -> FrameIndexInfo {
        match self.kind {
            NodeKind::Operand(OperandNodeKind::FrameIndex(fi)) => fi,
            _ => panic!(),
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self.kind, NodeKind::Operand(OperandNodeKind::Constant(_)))
    }

    pub fn is_frame_index(&self) -> bool {
        matches!(self.kind, NodeKind::Operand(OperandNodeKind::FrameIndex(_)))
    }

    pub fn is_maybe_register(&self) -> bool {
        self.is_operation()
            || matches!(self.kind,
                 NodeKind::Operand(OperandNodeKind::Address(_)) |
                 NodeKind::Operand(OperandNodeKind::Register(_)))
    }

    pub fn is_operation(&self) -> bool {
        match self.kind {
            NodeKind::Operand(OperandNodeKind::CondKind(_))
            | NodeKind::Operand(OperandNodeKind::FrameIndex(_))
            | NodeKind::Operand(OperandNodeKind::Constant(_))
            | NodeKind::Operand(OperandNodeKind::Address(_))
            | NodeKind::Operand(OperandNodeKind::BasicBlock(_))
            | NodeKind::Operand(OperandNodeKind::Register(_))
            | NodeKind::Operand(OperandNodeKind::Mem(_))
            | NodeKind::None => false,
            _ => true,
        }
    }

    pub fn may_contain_children(&self) -> bool {
        match self.kind {
            NodeKind::Operand(OperandNodeKind::CondKind(_))
            | NodeKind::Operand(OperandNodeKind::FrameIndex(_))
            | NodeKind::Operand(OperandNodeKind::Constant(_))
            | NodeKind::Operand(OperandNodeKind::Address(_))
            | NodeKind::Operand(OperandNodeKind::BasicBlock(_))
            | NodeKind::Operand(OperandNodeKind::Register(_))
            | NodeKind::None => false,
            _ => true,
        }
    }

    pub fn debug(
        &self,
        f: &mut fmt::Formatter<'_>,
        tys: &Types,
        s: &mut FxHashMap<Raw<DAGNode>, usize>,
        self_id: usize, // 0 is entry
        indent: usize,
    ) -> fmt::Result {
        #[rustfmt::skip]
        macro_rules! id4op { ($op:expr) => {{
            let l=s.len()+1; s.entry($op).or_insert(l)
        }}}
        write!(f, "{}", " ".repeat(indent))?;
        write!(f, "id{}({}) = ", self_id, tys.to_string(self.ty))?;
        write!(f, "{:?}", self.kind)?;
        for (i, op) in self.operand.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?
            }
            if op.kind == NodeKind::None {
                continue;
            }
            if op.may_contain_children() {
                write!(f, " id{}", id4op!(*op))?;
            } else {
                write!(f, " ")?;
                op.kind.as_operand().debug(f, tys)?;
            }
            write!(f, ".{}", tys.to_string(op.ty))?;
        }
        write!(f, "\n")?;
        for op in &self.operand {
            if !op.may_contain_children() || op.kind == NodeKind::None {
                continue;
            }
            let id = *id4op!(*op);
            op.debug(f, tys, s, id, indent + 2)?;
        }
        if let Some(next) = self.next {
            let id = *id4op!(next);
            next.debug(f, tys, s, id, indent)?;
        }
        fmt::Result::Ok(())
    }
}

impl OperandNodeKind {
    pub fn debug(&self, f: &mut fmt::Formatter<'_>, tys: &Types) -> fmt::Result {
        match self {
            Self::Address(a) => write!(f, "Addr({:?})", a),
            Self::BasicBlock(b) => write!(f, "BB#{}", b.index()),
            Self::CondKind(c) => write!(f, "Cond({:?})", c),
            Self::Constant(c) => write!(f, "{:?}", c),
            Self::FrameIndex(fi) => write!(f, "FI<{}, {:?}>", tys.to_string(fi.ty), fi.idx),
            Self::Register(r) => write!(f, "Reg({:?})", r),
            Self::Mem(mem) => write!(f, "{:?}", mem),
        }
    }
}

impl NodeKind {
    pub fn as_mi(&self) -> MINodeKind {
        match self {
            NodeKind::MI(mi) => *mi,
            _ => panic!(),
        }
    }

    pub fn as_operand(&self) -> &OperandNodeKind {
        match self {
            NodeKind::Operand(o) => o,
            _ => panic!(),
        }
    }
}

impl MINodeKind {
    pub fn get_def_type(&self) -> Type {
        rc2ty(self.inst_def().unwrap().defs[0].as_reg_class())
    }
}
