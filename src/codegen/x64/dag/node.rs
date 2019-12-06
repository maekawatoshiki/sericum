use super::super::machine::instr::*;
use super::basic_block::*;
use crate::ir::{opcode::*, types::*};
use crate::util::allocator::*;
use id_arena::*;

pub type DAGNodeId = Id<DAGNode>;

#[derive(Debug, Clone)]
pub struct DAGNode {
    pub kind: DAGNodeKind,
    pub operand: Vec<Raw<DAGNode>>,
    pub ty: Type,
    pub next: Option<Raw<DAGNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DAGNodeKind {
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

    LoadFiConstOff,
    LoadFiOff,
    LoadRegOff,
    StoreFiConstOff,
    StoreFiOff,
    StoreRegOff,

    CondKind(CondKind),
    FrameIndex(i32, Type), // TODO
    Constant(ConstantKind),
    GlobalAddress(GlobalValueKind),
    BasicBlock(DAGBasicBlockId),
    Register(RegisterInfoRef),
    CopyToLiveOut,

    None,
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
    pub fn new(kind: DAGNodeKind, operand: Vec<Raw<DAGNode>>, ty: Type) -> Self {
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
            DAGNodeKind::BasicBlock(bb) => bb,
            _ => panic!(),
        }
    }

    pub fn as_cond_kind(&self) -> CondKind {
        match self.kind {
            DAGNodeKind::CondKind(ck) => ck,
            _ => panic!(),
        }
    }

    pub fn as_constant(&self) -> ConstantKind {
        match self.kind {
            DAGNodeKind::Constant(c) => c,
            _ => panic!(),
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self.kind, DAGNodeKind::Constant(_))
    }

    pub fn is_frame_index(&self) -> bool {
        matches!(self.kind, DAGNodeKind::FrameIndex(_, _))
    }

    pub fn is_operation(&self) -> bool {
        match self.kind {
            DAGNodeKind::CondKind(_)
            | DAGNodeKind::FrameIndex(_, _)
            | DAGNodeKind::Constant(_)
            | DAGNodeKind::GlobalAddress(_)
            | DAGNodeKind::BasicBlock(_)
            | DAGNodeKind::None => false,
            _ => true,
        }
    }

    // pub fn to_dot(&self, self_id: DAGNodeId, arena: &Arena<DAGNode>) -> String {
    //     let mut s = "".to_string();
    //     let mut mark = FxHashSet::default();
    //     self.to_dot_sub(&mut s, &mut mark, self_id, arena);
    //     format!("digraph g {{ {} }}", s)
    // }
    //
    // fn to_dot_sub(
    //     &self,
    //     s: &mut String,
    //     mark: &mut FxHashSet<DAGNodeId>,
    //     self_id: DAGNodeId,
    //     arena: &Arena<DAGNode>,
    // ) {
    //     if !mark.insert(self_id) {
    //         return;
    //     }
    //
    //     match self.kind {
    //         DAGNodeKind::Entry => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Entry}}\"];",
    //                     self_id.index()
    //                 )
    //                 .as_str(),
    //             );
    //         }
    //         DAGNodeKind::Load(dagid) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Load|{1}}}\"];
    //                     instr{0} -> instr{2} [label=\"0\" color=\"#1E92FF\"];",
    //                     self_id.index(),
    //                     self.ty.as_ref().unwrap().to_string(),
    //                     dagid.index(),
    //                 )
    //                 .as_str(),
    //             );
    //             arena[dagid].to_dot_sub(s, mark, dagid, arena);
    //         }
    //         DAGNodeKind::Store(op1, op2) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Store}}\"];
    //                     instr{0} -> instr{1} [label=\"0\" color=\"#1E92FF\"];
    //                     instr{0} -> instr{2} [label=\"1\" color=\"#1E92FF\"];",
    //                     self_id.index(),
    //                     op1.index(),
    //                     op2.index(),
    //                 )
    //                 .as_str(),
    //             );
    //             arena[op1].to_dot_sub(s, mark, op1, arena);
    //             arena[op2].to_dot_sub(s, mark, op2, arena);
    //         }
    //         DAGNodeKind::Call(f, ref args) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Call|{}}}\"];",
    //                     self_id.index(),
    //                     self.ty.as_ref().unwrap().to_string(),
    //                 )
    //                 .as_str(),
    //             );
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} -> instr{} [label=\"callee\" color=\"#1E92FF\"];",
    //                     self_id.index(),
    //                     f.index(),
    //                 )
    //                 .as_str(),
    //             );
    //             arena[f].to_dot_sub(s, mark, f, arena);
    //             for (i, arg) in args.iter().enumerate() {
    //                 s.push_str(
    //                     format!(
    //                         "\ninstr{} -> instr{} [label=\"{}\" color=\"#1E92FF\"];",
    //                         self_id.index(),
    //                         arg.index(),
    //                         i + 1
    //                     )
    //                     .as_str(),
    //                 );
    //                 arena[*arg].to_dot_sub(s, mark, *arg, arena);
    //             }
    //         }
    //         DAGNodeKind::Add(op1, op2)
    //         | DAGNodeKind::Sub(op1, op2)
    //         | DAGNodeKind::Mul(op1, op2)
    //         | DAGNodeKind::Rem(op1, op2) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} [shape=record,shape=Mrecord,label=\"{{{}|{}}}\"];",
    //                     self_id.index(),
    //                     match self.kind {
    //                         DAGNodeKind::Add(_, _) => "Add",
    //                         DAGNodeKind::Sub(_, _) => "Sub",
    //                         DAGNodeKind::Mul(_, _) => "Mul",
    //                         DAGNodeKind::Rem(_, _) => "Rem",
    //                         _ => "",
    //                     },
    //                     self.ty.as_ref().unwrap().to_string(),
    //                 )
    //                 .as_str(),
    //             );
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{0} -> instr{1} [label=\"0\" color=\"#1E92FF\"];
    //                     instr{0} -> instr{2} [label=\"1\" color=\"#1E92FF\"];",
    //                     self_id.index(),
    //                     op1.index(),
    //                     op2.index(),
    //                 )
    //                 .as_str(),
    //             );
    //             arena[op1].to_dot_sub(s, mark, op1, arena);
    //             arena[op2].to_dot_sub(s, mark, op2, arena);
    //         }
    //         DAGNodeKind::Setcc(c, op1, op2) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Setcc|{:?}|{}}}\"];",
    //                     self_id.index(),
    //                     c,
    //                     self.ty.as_ref().unwrap().to_string(),
    //                 )
    //                 .as_str(),
    //             );
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} -> instr{} [label=\"0\" color=\"#1E92FF\"];
    //                     instr{} -> instr{} [label=\"1\" color=\"#1E92FF\"];",
    //                     self_id.index(),
    //                     op1.index(),
    //                     self_id.index(),
    //                     op2.index(),
    //                 )
    //                 .as_str(),
    //             );
    //             arena[op1].to_dot_sub(s, mark, op1, arena);
    //             arena[op2].to_dot_sub(s, mark, op2, arena);
    //         }
    //         DAGNodeKind::Phi(_) => {} // TODO
    //         DAGNodeKind::BrCond(v, bb) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{BrCond}}\"];
    //                     instr{0} -> instr{1} [label=\"0\" color=\"#1E92FF\"];
    //                     instr{0} -> branch{2} [color=\"#fe8833\"];",
    //                     self_id.index(),
    //                     v.index(),
    //                     bb.index()
    //                 )
    //                 .as_str(),
    //             );
    //             arena[v].to_dot_sub(s, mark, v, arena);
    //         }
    //         DAGNodeKind::Brcc(c, op0, op1, bb) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Br_cc|{1:?}}}\"];
    //                     instr{0} -> instr{2} [label=\"0\" color=\"#1E92FF\"];
    //                     instr{0} -> instr{3} [label=\"0\" color=\"#1E92FF\"];
    //                     instr{0} -> branch{4} [color=\"#fe8833\"];",
    //                     self_id.index(),
    //                     c,
    //                     op0.index(),
    //                     op1.index(),
    //                     bb.index(),
    //                 )
    //                 .as_str(),
    //             );
    //             arena[op0].to_dot_sub(s, mark, op0, arena);
    //             arena[op1].to_dot_sub(s, mark, op1, arena);
    //         }
    //         DAGNodeKind::Br(bb) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Br}}\"];
    //                     instr{0} -> branch{1} [color=\"#fe8833\"];",
    //                     self_id.index(),
    //                     bb.index()
    //                 )
    //                 .as_str(),
    //             );
    //         }
    //         DAGNodeKind::Ret(v) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Ret}}\"];
    //                     instr{0} -> instr{1} [label=\"0\" color=\"#1E92FF\"];",
    //                     self_id.index(),
    //                     v.index()
    //                 )
    //                 .as_str(),
    //             );
    //             arena[v].to_dot_sub(s, mark, v, arena);
    //         }
    //         DAGNodeKind::FrameIndex(i, ref ty) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} [shape=record,shape=Mrecord,label=\"{{FrameIndex:{}|{}}}\"];",
    //                     self_id.index(),
    //                     i,
    //                     ty.to_string()
    //                 )
    //                 .as_str(),
    //             );
    //         }
    //         // DAGNodeKind::Register(ref r) => {
    //         //     s.push_str(
    //         //         format!(
    //         //             "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Register:{:?}}}\"];",
    //         //             self_id.index(),
    //         //             r
    //         //         )
    //         //         .as_str(),
    //         //     );
    //         // }
    //         DAGNodeKind::Constant(ref c) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Constant:{:?}}}\"];",
    //                     self_id.index(),
    //                     c
    //                 )
    //                 .as_str(),
    //             );
    //         }
    //         DAGNodeKind::GlobalAddress(ref g) => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} [shape=record,shape=Mrecord,label=\"{{GlobalAddress:{:?}}}\"];",
    //                     self_id.index(),
    //                     g
    //                 )
    //                 .as_str(),
    //             );
    //         }
    //         DAGNodeKind::None => {
    //             s.push_str(
    //                 format!(
    //                     "\ninstr{} [shape=record,shape=Mrecord,label=\"{{None}}\"];",
    //                     self_id.index(),
    //                 )
    //                 .as_str(),
    //             );
    //         }
    //     }
    //
    //     some_then!(next, self.next, {
    //         s.push_str(
    //             format!(
    //                 "\ninstr{} -> instr{} [label=\"chain\"];",
    //                 self_id.index(),
    //                 next.index(),
    //             )
    //             .as_str(),
    //         );
    //         arena[next].to_dot_sub(s, mark, next, arena)
    //     });
    // }
}
