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

pub fn get_ordered_general_reg(n: usize) -> Option<PhysReg> {
    match n {
        0 => Some(PhysReg(0)),
        1 => Some(PhysReg(1)),
        2 => Some(PhysReg(2)),
        3 => Some(PhysReg(4)),
        4 => Some(PhysReg(5)),
        5 => Some(PhysReg(8)),
        6 => Some(PhysReg(9)),
        7 => Some(PhysReg(10)),
        8 => Some(PhysReg(11)),
        _ => None,
    }
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
            "EAX", "ECX", "EDX", "EBX", "ESI", "EDI", "ESP", "EBP", "R8D", "R9D", "R10D", "R11D",
            "R12D", "R13D", "R14D", "R15D",
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
