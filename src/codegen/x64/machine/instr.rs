use super::basic_block::*;
use crate::ir::types::*;
use id_arena::*;
// use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

pub type RegisterInfoRef = Rc<RefCell<RegisterInfo>>;
pub type MachineInstrId = Id<MachineInstr>;

#[derive(Debug, Clone)]
pub struct MachineInstr {
    pub opcode: MachineOpcode,
    pub operand: Vec<MachineOperand>,
    pub ty: Option<Type>,
    pub reg: RegisterInfoRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegisterInfo {
    pub vreg: usize,
    pub reg: Option<usize>,
    pub ty: Type,
    pub spill: bool,
    pub last_use: Option<MachineInstrId>,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum MachineOpcode {
    // Memory
    Load,
    Store,
    CopyToReg,

    // Call
    Call,

    // Binary arithmetics
    Add,
    Sub,

    // Comparison
    Seteq,
    Setle,

    // Branch
    BrCond,
    Br,
    BrccEq,
    BrccLe,

    // Phi
    Phi,

    // Return
    Ret,
}

#[derive(Debug, Clone)]
pub enum MachineOperand {
    // Instr(MachineInstrId),
    Register(MachineRegister),
    Constant(MachineConstant),
    FrameIndex(FrameIndexInfo),
    GlobalAddress(GlobalValueInfo),
    Branch(MachineBasicBlockId),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MachineConstant {
    Int32(i32),
}

#[derive(Debug, Clone)]
pub enum GlobalValueInfo {
    FunctionName(String),
}

#[derive(Debug, Clone)]
pub struct FrameIndexInfo {
    pub ty: Type,
    pub idx: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MachineRegister {
    pub info: RegisterInfoRef,
    // pub vreg: usize,
    // pub phy_reg: usize
}

impl ::std::cmp::Eq for MachineRegister {}
impl ::std::hash::Hash for MachineRegister {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.info.borrow().vreg)
    }
}

impl MachineInstr {
    pub fn new(opcode: MachineOpcode, operand: Vec<MachineOperand>, ty: Option<Type>) -> Self {
        Self {
            opcode,
            operand,
            reg: Rc::new(RefCell::new(RegisterInfo::new(
                ty.clone().unwrap_or(Type::Int32),
            ))),
            ty,
        }
    }

    pub fn set_vreg(&self, vreg: usize) {
        self.reg.borrow_mut().vreg = vreg;
    }

    pub fn set_last_use(&self, last_use: Option<MachineInstrId>) {
        self.reg.borrow_mut().last_use = last_use;
    }

    pub fn set_phy_reg(&self, reg: usize, spill: bool) {
        let mut reg_info = self.reg.borrow_mut();
        reg_info.reg = Some(reg);
        reg_info.spill = spill;
    }

    pub fn get_last_use(&self) -> Option<MachineInstrId> {
        self.reg.borrow().last_use
    }

    pub fn get_vreg(&self) -> usize {
        self.reg.borrow().vreg
    }

    pub fn get_reg(&self) -> Option<usize> {
        self.reg.borrow().reg
    }
}

impl FrameIndexInfo {
    pub fn new(ty: Type, idx: i32) -> Self {
        Self { ty, idx }
    }
}

impl MachineRegister {
    pub fn new(info: RegisterInfoRef) -> Self {
        Self { info }
    }

    pub fn set_vreg(&self, vreg: usize) {
        self.info.borrow_mut().vreg = vreg;
    }

    pub fn set_last_use(&self, last_use: Option<MachineInstrId>) {
        self.info.borrow_mut().last_use = last_use;
    }

    pub fn set_phy_reg(&self, reg: usize, spill: bool) {
        let mut reg_info = self.info.borrow_mut();
        reg_info.reg = Some(reg);
        reg_info.spill = spill;
    }

    pub fn get_last_use(&self) -> Option<MachineInstrId> {
        self.info.borrow().last_use
    }

    pub fn get_vreg(&self) -> usize {
        self.info.borrow().vreg
    }

    pub fn get_reg(&self) -> Option<usize> {
        self.info.borrow().reg
    }

    pub fn info_ref(&self) -> Ref<RegisterInfo> {
        self.info.borrow()
    }
}

impl RegisterInfo {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            vreg: 0,
            reg: None,
            spill: false,
            last_use: None,
        }
    }
}
