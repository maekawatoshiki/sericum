use id_arena::{Arena, Id};
use rustc_hash::FxHashSet;

pub trait BasicBlockTrait: Sized {
    fn get_preds(&self) -> &FxHashSet<Id<Self>>;
    fn get_succs(&self) -> &FxHashSet<Id<Self>>;
}

pub trait BasicBlocksTrait: Sized {
    type BB: BasicBlockTrait;
    fn get_arena(&self) -> &Arena<Self::BB>;
    fn get_order(&self) -> &Vec<Id<Self::BB>>;
}

#[derive(Debug, Clone)]
pub struct BasicBlocksIter<'a, T: BasicBlocksTrait> {
    basic_blocks: &'a T,
    nth: usize,
}

impl<'a, T: BasicBlocksTrait> BasicBlocksIter<'a, T> {
    pub fn new(basic_blocks: &'a T) -> Self {
        Self {
            basic_blocks,
            nth: 0,
        }
    }
}

impl<'a, T: BasicBlocksTrait> Iterator for BasicBlocksIter<'a, T> {
    type Item = (Id<T::BB>, &'a T::BB);

    fn next(&mut self) -> Option<Self::Item> {
        self.nth += 1;
        let id = *self.basic_blocks.get_order().get(self.nth - 1)?;
        Some((id, &self.basic_blocks.get_arena()[id]))
    }
}
