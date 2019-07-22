use crate::ir::types::*;
// use id_arena::*;
// use rustc_hash::{FxHashMap, FxHashSet};
use std::{cell::RefCell, rc::Rc};

pub type RegisterInfoRef = Rc<RefCell<RegisterInfo>>;

#[derive(Debug, Clone)]
pub struct LowerInstr {
    pub opcode: LowerOpcode,
    pub oprand: Vec<LowerOprand>,
    pub ty: Option<Type>,
    pub reg: RegisterInfoRef,
}

#[derive(Debug, Clone)]
pub struct RegisterInfo {
    pub vreg: usize,
    pub reg: Option<usize>,
    pub spill: bool,
    // pub last_use: Option<InstructionId>,
}

#[derive(Debug, Clone)]
pub enum LowerOpcode {
    Load,
    Store,
}

#[derive(Debug, Clone)]
pub enum LowerOprand {
    VReg(VRegInfo),
    Constant(ConstantKind),
    FrameIndex(FrameIndexInfo),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConstantKind {
    Int32(i32),
}

#[derive(Debug, Clone)]
pub struct FrameIndexInfo {
    pub ty: Type,
    pub idx: i32,
}

#[derive(Debug, Clone)]
pub struct VRegInfo {
    pub ty: Type,
    pub vreg: usize,
}

impl LowerInstr {
    pub fn new(
        opcode: LowerOpcode,
        oprand: Vec<LowerOprand>,
        ty: Option<Type>,
        vreg: usize,
    ) -> Self {
        Self {
            opcode,
            oprand,
            ty,
            reg: Rc::new(RefCell::new(RegisterInfo::new(vreg))),
        }
    }
}

impl FrameIndexInfo {
    pub fn new(ty: Type, idx: i32) -> Self {
        Self { ty, idx }
    }
}

impl VRegInfo {
    pub fn new(ty: Type, vreg: usize) -> Self {
        Self { ty, vreg }
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
