use super::machine::instr::*;
use crate::ir::types::*;
use std::fmt;
use std::{cell::RefCell, rc::Rc};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct PhysReg(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct VirtReg(pub usize);

#[derive(Debug, Clone)]
pub struct VirtRegGen {
    id: Rc<RefCell<usize>>,
}

impl VirtRegGen {
    pub fn new() -> Self {
        Self {
            id: Rc::new(RefCell::new(0)),
        }
    }

    pub fn gen_vreg(&self, ty: Type) -> RegisterInfo {
        let mut reg = RegisterInfo::new(ty);
        reg.set_vreg(VirtReg(self.next_id()));
        reg
    }

    fn next_id(&self) -> usize {
        let mut id = self.id.borrow_mut();
        *id += 1;
        *id
    }
}

// register nubmering: https://corsix.github.io/dynasm-doc/instructions.html#registers

pub fn get_general_reg(n: usize) -> Option<PhysReg> {
    match n {
        0 => Some(PhysReg(0)),
        1 => Some(PhysReg(1)),
        2 => Some(PhysReg(2)),
        3 => Some(PhysReg(6)),
        4 => Some(PhysReg(7)),
        5 => Some(PhysReg(8)),
        6 => Some(PhysReg(9)),
        7 => Some(PhysReg(10)),
        8 => Some(PhysReg(11)),
        _ => None,
    }
}

pub fn get_arg_reg(n: usize) -> Option<PhysReg> {
    let regs = [7, 6, 2, 1, 8, 9]; // rdi, rsi, rdx, rcx, r8, r9
    regs.get(n).map(|x| PhysReg(*x))
}

impl PhysReg {
    pub fn get(&self) -> usize {
        self.0
    }
}

impl VirtReg {
    pub fn get(&self) -> usize {
        self.0
    }
}

impl fmt::Debug for PhysReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reg_names = [
            "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8", "R9", "R10", "R11",
            "R12", "R13", "R14", "R15",
        ];
        write!(f, "%{}", reg_names[self.0])
        // write!(f, "%R{}", self.0)
    }
}

impl fmt::Debug for VirtReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%vreg{}", self.0)
    }
}
