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
        write!(f, "%R{}", self.0)
    }
}

impl fmt::Debug for VirtReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%vreg{}", self.0)
    }
}
