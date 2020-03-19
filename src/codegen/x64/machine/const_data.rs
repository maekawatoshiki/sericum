use super::instr::MachineConstant;
use std::fmt;
use std::ops::{Index, IndexMut};
use std::sync::atomic::{self, AtomicUsize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DataId {
    arena_id: usize,
    id: usize,
}

pub struct ConstDataArena {
    id: usize,
    arena: Vec<MachineConstant>,
}

pub struct ConstDataArenaIter<'a> {
    id: usize,
    arena: &'a Vec<MachineConstant>,
    nth: usize,
}

impl ConstDataArena {
    pub fn new() -> Self {
        Self {
            id: Self::new_arena_id(),
            arena: vec![],
        }
    }

    pub fn alloc(&mut self, c: MachineConstant) -> DataId {
        let id = self.arena.len();
        self.arena.push(c);
        DataId {
            arena_id: self.id,
            id,
        }
    }

    pub fn id_and_data<'a>(&'a self) -> ConstDataArenaIter<'a> {
        ConstDataArenaIter::new(self.id, &self.arena)
    }

    fn new_arena_id() -> usize {
        static ARENA_COUNTER: AtomicUsize = AtomicUsize::new(0);
        ARENA_COUNTER.fetch_add(1, atomic::Ordering::SeqCst)
    }
}

impl<'a> ConstDataArenaIter<'a> {
    pub fn new(id: usize, arena: &'a Vec<MachineConstant>) -> Self {
        Self { id, arena, nth: 0 }
    }
}

impl<'a> Iterator for ConstDataArenaIter<'a> {
    type Item = (DataId, &'a MachineConstant);

    fn next(&mut self) -> Option<Self::Item> {
        self.nth += 1;
        let id = self.nth - 1;
        self.arena.get(id).and_then(|item| {
            Some((
                DataId {
                    id,
                    arena_id: self.id,
                },
                item,
            ))
        })
    }
}

impl Index<DataId> for ConstDataArena {
    type Output = MachineConstant;

    fn index(&self, id: DataId) -> &Self::Output {
        assert_eq!(self.id, id.arena_id);
        &self.arena[id.id]
    }
}

impl IndexMut<DataId> for ConstDataArena {
    fn index_mut(&mut self, id: DataId) -> &mut Self::Output {
        assert_eq!(self.id, id.arena_id);
        &mut self.arena[id.id]
    }
}

impl fmt::Display for DataId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "data:{}", self.id)
    }
}
