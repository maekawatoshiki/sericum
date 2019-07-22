use super::basic_block::*;
use crate::ir::{basic_block::*, opcode::*, types::*};
use id_arena::*;
use rustc_hash::FxHashSet;
use std::{cell::RefCell, rc::Rc};

pub type DAGNodeId = Id<DAGNode>;

#[derive(Debug, Clone)]
pub struct DAGNode {
    pub kind: DAGNodeKind,
    pub ty: Option<Type>,
    pub next: Option<DAGNodeId>,
    pub reg: RegisterInfoRef,
}

pub type RegisterInfoRef = Rc<RefCell<RegisterInfo>>;

#[derive(Debug, Clone)]
pub struct RegisterInfo {
    pub vreg: usize,
    pub reg: Option<usize>,
    pub spill: bool,
    // pub last_use: Option<InstructionId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DAGNodeKind {
    Entry,

    Load(DAGNodeId),
    Store(DAGNodeId, DAGNodeId), // dst, src
    Add(DAGNodeId, DAGNodeId),
    Setcc(CondKind, DAGNodeId, DAGNodeId),
    BrCond(DAGNodeId, DAGBasicBlockId),
    Br(DAGBasicBlockId),
    Ret(DAGNodeId),

    FrameIndex(i32, Type),
    // Register(RegisterKind),
    Constant(ConstantKind),
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum RegisterKind {
//     VReg(VirtualRegister),
//     Reg(usize),
// }

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantKind {
    Int32(i32),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CondKind {
    Eq,
    Le,
}

impl Into<CondKind> for ICmpKind {
    fn into(self) -> CondKind {
        match self {
            ICmpKind::Eq => CondKind::Eq,
            ICmpKind::Le => CondKind::Le,
        }
    }
}

impl DAGNode {
    pub fn new(kind: DAGNodeKind, ty: Option<Type>) -> Self {
        Self {
            kind,
            ty,
            next: None,
            reg: Rc::new(RefCell::new(RegisterInfo::new(0))),
        }
    }

    pub fn set_next(mut self, next: DAGNodeId) -> Self {
        self.next = Some(next);
        self
    }

    pub fn to_dot(&self, self_id: DAGNodeId, arena: &Arena<DAGNode>) -> String {
        let mut s = "".to_string();
        let mut mark = FxHashSet::default();
        self.to_dot_sub(&mut s, &mut mark, self_id, arena);
        format!("digraph g {{ {} }}", s)
    }

    fn to_dot_sub(
        &self,
        s: &mut String,
        mark: &mut FxHashSet<DAGNodeId>,
        self_id: DAGNodeId,
        arena: &Arena<DAGNode>,
    ) {
        if !mark.insert(self_id) {
            return;
        }

        match self.kind {
            DAGNodeKind::Entry => {
                s.push_str(
                    format!(
                        "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Entry}}\"];",
                        self_id.index()
                    )
                    .as_str(),
                );
            }
            DAGNodeKind::Load(dagid) => {
                s.push_str(
                    format!(
                        "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Load|{1}}}\"];
                        instr{0} -> instr{2} [label=\"0\" color=\"#1E92FF\"];",
                        self_id.index(),
                        self.ty.as_ref().unwrap().to_string(),
                        dagid.index(),
                    )
                    .as_str(),
                );
                arena[dagid].to_dot_sub(s, mark, dagid, arena);
            }
            DAGNodeKind::Store(op1, op2) => {
                s.push_str(
                    format!(
                        "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Store}}\"];
                        instr{0} -> instr{1} [label=\"0\" color=\"#1E92FF\"];
                        instr{0} -> instr{2} [label=\"1\" color=\"#1E92FF\"];",
                        self_id.index(),
                        op1.index(),
                        op2.index(),
                    )
                    .as_str(),
                );
                arena[op1].to_dot_sub(s, mark, op1, arena);
                arena[op2].to_dot_sub(s, mark, op2, arena);
            }
            DAGNodeKind::Add(op1, op2) => {
                s.push_str(
                    format!(
                        "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Add|{}}}\"];",
                        self_id.index(),
                        self.ty.as_ref().unwrap().to_string(),
                    )
                    .as_str(),
                );
                s.push_str(
                    format!(
                        "\ninstr{0} -> instr{1} [label=\"0\" color=\"#1E92FF\"];
                        instr{0} -> instr{2} [label=\"1\" color=\"#1E92FF\"];",
                        self_id.index(),
                        op1.index(),
                        op2.index(),
                    )
                    .as_str(),
                );
                arena[op1].to_dot_sub(s, mark, op1, arena);
                arena[op2].to_dot_sub(s, mark, op2, arena);
            }
            DAGNodeKind::Setcc(c, op1, op2) => {
                s.push_str(
                    format!(
                        "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Setcc|{:?}|{}}}\"];",
                        self_id.index(),
                        c,
                        self.ty.as_ref().unwrap().to_string(),
                    )
                    .as_str(),
                );
                s.push_str(
                    format!(
                        "\ninstr{} -> instr{} [label=\"0\" color=\"#1E92FF\"];
                        instr{} -> instr{} [label=\"1\" color=\"#1E92FF\"];",
                        self_id.index(),
                        op1.index(),
                        self_id.index(),
                        op2.index(),
                    )
                    .as_str(),
                );
                arena[op1].to_dot_sub(s, mark, op1, arena);
                arena[op2].to_dot_sub(s, mark, op2, arena);
            }
            DAGNodeKind::BrCond(v, bb) => {
                s.push_str(
                    format!(
                        "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{BrCond}}\"];
                        instr{0} -> instr{1} [label=\"0\" color=\"#1E92FF\"];
                        instr{0} -> branch{2} [color=\"#fe8833\"];",
                        self_id.index(),
                        v.index(),
                        bb.index()
                    )
                    .as_str(),
                );
                arena[v].to_dot_sub(s, mark, v, arena);
            }
            DAGNodeKind::Br(bb) => {
                s.push_str(
                    format!(
                        "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Br}}\"];
                        instr{0} -> branch{1} [color=\"#fe8833\"];",
                        self_id.index(),
                        bb.index()
                    )
                    .as_str(),
                );
            }
            DAGNodeKind::Ret(v) => {
                s.push_str(
                    format!(
                        "\ninstr{0} [shape=record,shape=Mrecord,label=\"{{Ret}}\"];
                        instr{0} -> instr{1} [label=\"0\" color=\"#1E92FF\"];",
                        self_id.index(),
                        v.index()
                    )
                    .as_str(),
                );
                arena[v].to_dot_sub(s, mark, v, arena);
            }
            DAGNodeKind::FrameIndex(i, ref ty) => {
                s.push_str(
                    format!(
                        "\ninstr{} [shape=record,shape=Mrecord,label=\"{{FrameIndex:{}|{}}}\"];",
                        self_id.index(),
                        i,
                        ty.to_string()
                    )
                    .as_str(),
                );
            }
            // DAGNodeKind::Register(ref r) => {
            //     s.push_str(
            //         format!(
            //             "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Register:{:?}}}\"];",
            //             self_id.index(),
            //             r
            //         )
            //         .as_str(),
            //     );
            // }
            DAGNodeKind::Constant(ref c) => {
                s.push_str(
                    format!(
                        "\ninstr{} [shape=record,shape=Mrecord,label=\"{{Constant:{:?}}}\"];",
                        self_id.index(),
                        c
                    )
                    .as_str(),
                );
            }
        }

        some_then!(next, self.next, {
            s.push_str(
                format!(
                    "\ninstr{} -> instr{} [label=\"chain\"];",
                    self_id.index(),
                    next.index(),
                )
                .as_str(),
            );
            arena[next].to_dot_sub(s, mark, next, arena)
        });
    }
}

impl RegisterInfo {
    pub fn new(vreg: usize) -> Self {
        Self {
            vreg,
            reg: None,
            spill: false,
        }
    }
}
