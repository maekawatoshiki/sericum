use super::{basic_block::*, frame_object::*};
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

    LoadFiConstOff,
    LoadFiOff,
    LoadRegOff,
    StoreFiConstOff,
    StoreFiOff,
    StoreRegOff,

    // Call
    Call,

    // Binary arithmetics
    Add,
    Sub,
    Mul,
    Rem,

    // Comparison
    Seteq,
    Setle,
    Setlt,

    // Branch
    BrCond,
    Br,
    BrccEq,
    BrccLe,
    BrccLt,

    // Phi
    Phi,

    // Return
    Ret,
}

#[derive(Debug, Clone)]
pub enum MachineOperand {
    Register(MachineRegister),
    Constant(MachineConstant),
    FrameIndex(FrameIndexInfo),
    GlobalAddress(GlobalValueInfo),
    Branch(MachineBasicBlockId),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MachineConstant {
    Int32(i32),
}

#[derive(Debug, Clone)]
pub enum GlobalValueInfo {
    FunctionName(String),
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

impl MachineOpcode {
    pub fn is_terminator(&self) -> bool {
        match self {
            MachineOpcode::Ret
            | MachineOpcode::Br
            | MachineOpcode::BrCond
            | MachineOpcode::BrccEq
            | MachineOpcode::BrccLe => true,
            _ => false,
        }
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

impl MachineOperand {
    pub fn as_frame_index(&self) -> &FrameIndexInfo {
        match self {
            MachineOperand::FrameIndex(fi) => fi,
            _ => panic!(),
        }
    }

    pub fn as_register(&self) -> &MachineRegister {
        match self {
            MachineOperand::Register(r) => r,
            _ => panic!(),
        }
    }

    pub fn as_constant(&self) -> &MachineConstant {
        match self {
            MachineOperand::Constant(c) => c,
            _ => panic!(),
        }
    }
}
