use super::machine::instr::*;
use crate::ir::types::*;
use std::{cell::RefCell, rc::Rc};

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
        reg.set_vreg(self.next_id());
        reg
    }

    fn next_id(&self) -> usize {
        let mut id = self.id.borrow_mut();
        *id += 1;
        *id
    }
}
