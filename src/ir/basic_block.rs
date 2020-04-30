use super::{module::Module, opcode::*, value::*, DumpToString};
use id_arena::*;
use rustc_hash::FxHashSet;
use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

pub type BasicBlockId = Id<BasicBlock>;

pub trait BasicBlockTrait: Sized {
    fn get_preds(&self) -> &Vec<Id<Self>>;
    fn get_succs(&self) -> &Vec<Id<Self>>;
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

#[derive(Debug, Clone)]
pub struct BasicBlocks {
    pub arena: Arena<BasicBlock>,
    pub order: Vec<BasicBlockId>,
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    /// Information for liveness analysis
    // TODO: Will be removed
    pub liveness: Rc<RefCell<LivenessInfo>>,

    /// Predecessors
    pub pred: Vec<BasicBlockId>,

    /// Successors
    pub succ: Vec<BasicBlockId>,

    /// Instruction list
    pub iseq: Rc<RefCell<Vec<Value>>>,
}

#[derive(Clone, Debug)]
pub struct LivenessInfo {
    pub def: FxHashSet<InstructionId>,
    pub live_in: FxHashSet<InstructionId>,
    pub live_out: FxHashSet<InstructionId>,
}

impl BasicBlocks {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            order: vec![],
        }
    }
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: Rc::new(RefCell::new(Vec::new())),
            pred: vec![],
            succ: vec![],
            liveness: Rc::new(RefCell::new(LivenessInfo::new())),
        }
    }

    pub fn iseq_ref<'b>(&'b self) -> Ref<Vec<Value>> {
        self.iseq.borrow()
    }

    pub fn iseq_ref_mut(&self) -> RefMut<Vec<Value>> {
        self.iseq.borrow_mut()
    }

    pub fn find_inst_pos(&self, id2find: InstructionId) -> Option<usize> {
        self.iseq_ref()
            .iter()
            .enumerate()
            .find(|(_, val)| match val {
                Value::Instruction(i) if i.id == id2find => true,
                _ => false,
            })
            .map(|(i, _)| i)
    }
}

impl BasicBlockTrait for BasicBlock {
    fn get_preds(&self) -> &Vec<Id<Self>> {
        &self.pred
    }

    fn get_succs(&self) -> &Vec<Id<Self>> {
        &self.succ
    }
}

impl BasicBlocksTrait for BasicBlocks {
    type BB = BasicBlock;

    fn get_arena(&self) -> &Arena<Self::BB> {
        &self.arena
    }

    fn get_order(&self) -> &Vec<Id<Self::BB>> {
        &self.order
    }
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

impl DumpToString for BasicBlock {
    fn dump(&self, module: &Module) -> String {
        self.iseq_ref().iter().fold("".to_string(), |s, inst| {
            format!("{}{}\n", s, inst.to_string(module, true))
        })
    }
}

impl LivenessInfo {
    pub fn new() -> Self {
        Self {
            def: FxHashSet::default(),
            live_in: FxHashSet::default(),
            live_out: FxHashSet::default(),
        }
    }
}
