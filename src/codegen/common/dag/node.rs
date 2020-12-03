use crate::codegen::{
    arch::{
        dag::node::MemKind,
        frame_object::FrameIndexInfo,
        machine::{inst::MachineOpcode, register::*},
    },
    common::{dag::basic_block::DAGBasicBlockId, types::MVType},
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
    pub reg_class: Option<RegisterClassKind>,
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
    Mem(MemKind),
    CC(CondKind),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ImmediateKind {
    Int8(i8),
    Int32(i32),
    Int64(i64),
    F64(f64),
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

impl MINode {
    pub fn new(opcode: MachineOpcode) -> Self {
        Self {
            opcode,
            args: vec![],
            reg_class: None,
            next: None,
            chain: None,
        }
    }

    pub fn args(mut self, args: Vec<NodeId>) -> Self {
        self.args = args;
        self
    }

    pub fn reg_class(mut self, rc: RegisterClassKind) -> Self {
        self.reg_class = Some(rc);
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

    pub fn as_addr(&self) -> &AddressKind {
        match self {
            Self::Addr(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_cc(&self) -> &CondKind {
        match self {
            Self::CC(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_cc_mut(&mut self) -> &mut CondKind {
        match self {
            Self::CC(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_block(&self) -> &DAGBasicBlockId {
        match self {
            Self::Block(x) => x,
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

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Int8(0) | Self::Int32(0) | Self::Int64(0))
            || matches!(self, Self::F64(f) if *f == 0.0)
    }

    pub fn bits_within(&self, n: u32) -> Option<bool> {
        match self {
            Self::Int8(x) => Some((x << (8 - n)) >> (8 - n) == *x),
            Self::Int32(x) => Some((x << (32 - n)) >> (32 - n) == *x),
            Self::Int64(x) => Some((x << (64 - n)) >> (64 - n) == *x),
            Self::F64(_) => None,
        }
    }
}

impl AddressKind {
    pub fn as_func_name(&self) -> &String {
        match self {
            Self::FunctionName(name) => name,
            _ => panic!(),
        }
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
    Zext,
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

impl Into<Node> for MINode {
    fn into(self) -> Node {
        Node::MI(self)
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

impl Into<Node> for MemKind {
    fn into(self) -> Node {
        Node::Operand(OperandNode::Mem(self))
    }
}

impl Into<Node> for RegisterId {
    fn into(self) -> Node {
        Node::Operand(OperandNode::Reg(self))
    }
}

use rustc_hash::FxHashMap;
use std::fmt;

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

    pub fn as_mi(&self) -> &MINode {
        match self {
            Node::MI(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_operand(&self) -> &OperandNode {
        match self {
            Node::Operand(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_operand_mut(&mut self) -> &mut OperandNode {
        match self {
            Node::Operand(x) => x,
            _ => panic!(),
        }
    }

    pub fn as_i32(&self) -> i32 {
        self.as_operand().as_imm().as_i32()
    }

    pub fn args(&self) -> &[NodeId] {
        match self {
            Self::IR(ir) => &ir.args,
            Self::MI(mi) => &mi.args,
            Self::Operand(OperandNode::Mem(m)) => m.args(),
            Self::Operand(_) => &[],
            Self::None => &[],
        }
    }

    pub fn args_mut(&mut self) -> &mut [NodeId] {
        match self {
            Self::IR(ir) => &mut ir.args,
            Self::MI(mi) => &mut mi.args,
            Self::Operand(OperandNode::Mem(m)) => m.args_mut(),
            Self::Operand(_) => &mut [],
            Self::None => &mut [],
        }
    }

    pub fn next(&self) -> Option<NodeId> {
        match self {
            Self::IR(IRNode { next, .. }) | Self::MI(MINode { next, .. }) => *next,
            _ => None,
        }
    }

    pub fn next_mut(&mut self) -> &mut Option<NodeId> {
        match self {
            Self::IR(IRNode { next, .. }) | Self::MI(MINode { next, .. }) => next,
            _ => panic!(),
        }
    }

    pub fn debug(
        &self,
        f: &mut fmt::Formatter<'_>,
        arena: &Arena<Node>,
        tys: &Types,
        s: &mut FxHashMap<NodeId, (usize, bool)>,
        self_id: usize, // 0 is entry
        indent: usize,
    ) -> fmt::Result {
        #[rustfmt::skip]
        macro_rules! id4op { ($op:expr) => {{
            let l=s.len()+1; s.entry($op).or_insert((l, false))
        }}}
        write!(f, "{}", " ".repeat(indent))?;
        write!(
            f,
            "id{}({}) = ",
            self_id,
            match self {
                Self::IR(IRNode { ty, .. }) => tys.to_string(*ty),
                Self::MI(MINode {
                    reg_class: Some(rc),
                    ..
                }) => format!("{:?}", rc),
                _ => "".to_string(),
            }
        )?;
        match self {
            Self::IR(IRNode { opcode, .. }) => write!(f, "{:?}", opcode)?,
            Self::MI(MINode { opcode, .. }) => write!(f, "{:?}", opcode)?,
            Self::Operand(_) => panic!(),
            Self::None => panic!(),
        };
        for (i, id) in self.args().iter().enumerate() {
            let op = &arena[*id];
            if i > 0 {
                write!(f, ",")?
            }
            if matches!(op, Node::None) {
                write!(f, " None")?;
                continue;
            }
            match op {
                Node::Operand(op) => {
                    write!(f, " ")?;
                    op.debug(f, arena, s, tys)?;
                }
                _ => write!(f, " id{}", id4op!(*id).0)?,
            }
        }
        write!(f, "\n")?;
        for &op in self.args() {
            match &arena[op] {
                Self::Operand(OperandNode::Mem(mem)) => {
                    for &arg in mem.args() {
                        match &arena[arg] {
                            Self::Operand(_) | Self::None => continue,
                            node => {
                                let id = id4op!(arg).0;
                                if !s[&arg].1 {
                                    s.get_mut(&arg).unwrap().1 = true;
                                    node.debug(f, arena, tys, s, id, indent + 2)?;
                                }
                            }
                        };
                    }
                }
                Self::Operand(_) | Self::None => {
                    continue;
                }
                node => {
                    let id = id4op!(op).0;
                    if !s[&op].1 {
                        s.get_mut(&op).unwrap().1 = true;
                        node.debug(f, arena, tys, s, id, indent + 2)?;
                    }
                }
            }
        }
        if let Self::IR(IRNode {
            next: Some(next), ..
        })
        | Self::MI(MINode {
            next: Some(next), ..
        }) = self
        {
            let id = id4op!(*next).0;
            s.get_mut(next).unwrap().1 = true;
            arena[*next].debug(f, arena, tys, s, id, indent)?;
        }

        Ok(())
    }
}

impl OperandNode {
    pub fn debug(
        &self,
        f: &mut fmt::Formatter<'_>,
        arena: &Arena<Node>,
        s: &mut FxHashMap<NodeId, (usize, bool)>,
        tys: &Types,
    ) -> fmt::Result {
        #[rustfmt::skip]
        macro_rules! id4op { ($op:expr) => {{ let l=s.len()+1; s.entry($op).or_insert((l, false)) }}}
        match self {
            Self::Addr(a) => write!(f, "Addr({:?})", a),
            Self::Block(b) => write!(f, "BB#{}", b.index()),
            Self::CC(c) => write!(f, "Cond({:?})", c),
            Self::Imm(c) => write!(f, "{:?}", c),
            Self::Slot(fi) => write!(f, "FI<{}, {:?}>", tys.to_string(fi.ty), fi.idx),
            Self::Reg(r) => write!(f, "Reg({:?})", r),
            Self::Mem(mem) => {
                write!(f, "{:?}(", mem)?;
                let args = mem.args();
                for (i, &a) in args.iter().enumerate() {
                    match &arena[a] {
                        Node::Operand(op) => op.debug(f, arena, s, tys)?,
                        _ => write!(f, "id{}", id4op!(a).0)?,
                    }
                    if i < args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }
        }
    }
}
