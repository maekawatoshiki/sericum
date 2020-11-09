use crate::codegen::arch::machine::inst::TargetOpcode;
use crate::codegen::arch::machine::register::{PhysReg, RegisterClassKind};
use rustc_hash::FxHashMap;

#[derive(Clone)]
pub struct TargetInstDef {
    pub name: &'static str,
    pub opcode: TargetOpcode,
    pub uses: Vec<TargetOperand>,
    pub defs: Vec<TargetRegister>,
    pub tie: FxHashMap<DefOrUseReg, DefOrUseReg>, // def -> use
    pub imp_use: Vec<TargetRegister>,
    pub imp_def: Vec<TargetRegister>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum DefOrUseReg {
    Def(usize), // nth def register
    Use(usize), // nth use register
}

#[derive(Clone)]
pub enum TargetOperand {
    Register(TargetRegister),
    Immediate(TargetImmediate),
    Addr,
    FrameIndex,
    Any,
    Mem,
    Block,
}

use std::{fmt::Debug, hash::Hash};
#[derive(Clone)]
pub enum TargetRegister {
    RegClass(RegisterClassKind),
    Specific(PhysReg),
    Any,
}

#[derive(Clone, Copy, PartialEq)]
pub enum TargetImmediate {
    I8,
    I16,
    I32,
    I64,
    F64,
}

impl TargetInstDef {
    pub fn new(name: &'static str, opcode: TargetOpcode) -> Self {
        Self {
            name,
            opcode,
            uses: vec![],
            defs: vec![],
            tie: FxHashMap::default(),
            imp_def: vec![],
            imp_use: vec![],
        }
    }

    pub fn set_uses(mut self, uses: Vec<TargetOperand>) -> Self {
        self.uses = uses;
        self
    }

    pub fn set_defs(mut self, defs: Vec<TargetRegister>) -> Self {
        self.defs = defs;
        self
    }

    pub fn add_tie(mut self, def: DefOrUseReg, use_: DefOrUseReg) -> Self {
        self.tie.insert(def, use_);
        self
    }

    pub fn set_imp_def(mut self, imp_def: Vec<TargetRegister>) -> Self {
        self.imp_def = imp_def;
        self
    }

    pub fn set_imp_use(mut self, imp_use: Vec<TargetRegister>) -> Self {
        self.imp_use = imp_use;
        self
    }

    pub fn def_reg_class(&self) -> RegisterClassKind {
        self.defs[0].as_reg_class()
    }
}

impl TargetRegister {
    pub fn as_reg_class(&self) -> RegisterClassKind {
        match self {
            Self::RegClass(rc) => *rc,
            _ => panic!(),
        }
    }
}

impl DefOrUseReg {
    pub fn as_def(&self) -> usize {
        match self {
            Self::Def(d) => *d,
            _ => panic!(),
        }
    }

    pub fn as_use(&self) -> usize {
        match self {
            Self::Use(u) => *u,
            _ => panic!(),
        }
    }
}
