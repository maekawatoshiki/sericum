// use super::register::*;
use rustc_hash::FxHashMap;

#[derive(Clone)]
pub struct TargetInstDef<OP, R> {
    pub name: &'static str,
    pub opcode: OP,
    pub uses: Vec<TargetOperand<R>>,
    pub defs: Vec<R>,
    pub tie: FxHashMap<DefOrUseReg, DefOrUseReg>, // def -> use
    pub imp_use: Vec<R>,
    pub imp_def: Vec<R>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum DefOrUseReg {
    Def(usize), // nth def register
    Use(usize), // nth use register
}

#[derive(Clone)]
pub enum TargetOperand<R> {
    Register(R),
    Immediate(TargetImmediate),
    Addr,
    FrameIndex,
    Any,
    Mem,
    Block,
}

use std::{fmt::Debug, hash::Hash};
#[derive(Clone)]
pub enum TargetRegister<
    RC: Debug + Clone + Copy + Hash + PartialEq,
    P: Debug + Clone + Copy + Hash + PartialEq,
> {
    RegClass(RC),
    Specific(P),
}

#[derive(Clone, Copy, PartialEq)]
pub enum TargetImmediate {
    I8,
    I32,
    I64,
    F64,
}

impl<OP, R> TargetInstDef<OP, R> {
    pub fn new(name: &'static str, opcode: OP) -> Self {
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

    pub fn set_uses(mut self, uses: Vec<TargetOperand<R>>) -> Self {
        self.uses = uses;
        self
    }

    pub fn set_defs(mut self, defs: Vec<R>) -> Self {
        self.defs = defs;
        self
    }

    pub fn add_tie(mut self, def: DefOrUseReg, use_: DefOrUseReg) -> Self {
        self.tie.insert(def, use_);
        self
    }

    pub fn set_imp_def(mut self, imp_def: Vec<R>) -> Self {
        self.imp_def = imp_def;
        self
    }

    pub fn set_imp_use(mut self, imp_use: Vec<R>) -> Self {
        self.imp_use = imp_use;
        self
    }
}

impl<RC: Debug + Clone + Copy + Hash + PartialEq, P: Debug + Clone + Copy + Hash + PartialEq>
    TargetRegister<RC, P>
{
    pub fn as_reg_class(&self) -> RC {
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
