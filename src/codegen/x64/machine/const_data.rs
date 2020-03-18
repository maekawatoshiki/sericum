use super::instr::MachineConstant;
// use crate::ir::types::*;
// use rustc_hash::FxHashMap;
// use std::fmt;
use std::ops::{Index, IndexMut};

pub type DataId = usize;

pub struct ConstDataArena {
    pub arena: Vec<MachineConstant>,
}

impl ConstDataArena {
    pub fn new() -> Self {
        Self { arena: vec![] }
    }

    pub fn alloc(&mut self, c: MachineConstant) -> DataId {
        let id = self.arena.len();
        self.arena.push(c);
        id
    }
}

impl Index<DataId> for ConstDataArena {
    type Output = MachineConstant;

    fn index(&self, id: DataId) -> &Self::Output {
        &self.arena[id]
    }
}

impl IndexMut<DataId> for ConstDataArena {
    fn index_mut(&mut self, id: DataId) -> &mut Self::Output {
        &mut self.arena[id]
    }
}
