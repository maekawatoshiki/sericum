use super::machine::instr::*;
use crate::ir::types::*;

#[derive(Debug, Clone)]
pub struct VirtRegGen {
    pub id: usize,
}

impl VirtRegGen {
    pub fn new() -> Self {
        Self { id: 0 }
    }

    pub fn gen_vreg(&mut self, ty: Type) -> RegisterInfo {
        let mut reg = RegisterInfo::new(ty);
        reg.set_vreg(self.next_id());
        reg
    }

    fn next_id(&mut self) -> usize {
        self.id += 1;
        self.id
    }
}
