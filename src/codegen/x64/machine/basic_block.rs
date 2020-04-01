// use super::{module::*, opcode::*, value::*};
use super::inst::*;
use id_arena::*;
use rustc_hash::FxHashSet;
use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

pub type MachineBasicBlockId = Id<MachineBasicBlock>;

#[derive(Debug, Clone)]
pub struct MachineBasicBlocks {
    pub arena: Arena<MachineBasicBlock>,
    pub order: Vec<MachineBasicBlockId>,
}

#[derive(Debug, Clone)]
pub struct MachineBasicBlocksIter<'a> {
    basic_blocks: &'a MachineBasicBlocks,
    nth: usize,
}

#[derive(Clone, Debug)]
pub struct MachineBasicBlock {
    /// Information for liveness analysis
    // TODO: Will be removed
    pub liveness: Rc<RefCell<LivenessInfo>>,

    /// Predecessors
    pub pred: Vec<MachineBasicBlockId>,

    /// Successors
    pub succ: Vec<MachineBasicBlockId>,

    /// Instruction list
    pub iseq: RefCell<Vec<MachineInstId>>,
}

#[derive(Clone, Debug)]
pub struct LivenessInfo {
    pub def: FxHashSet<MachineRegister>,
    pub live_in: FxHashSet<MachineRegister>,
    pub live_out: FxHashSet<MachineRegister>,
}

impl MachineBasicBlocks {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            order: vec![],
        }
    }

    pub fn id_and_block(&self) -> MachineBasicBlocksIter {
        MachineBasicBlocksIter::new(self)
    }
}

impl<'a> MachineBasicBlocksIter<'a> {
    pub fn new(basic_blocks: &'a MachineBasicBlocks) -> Self {
        Self {
            basic_blocks,
            nth: 0,
        }
    }
}

impl<'a> Iterator for MachineBasicBlocksIter<'a> {
    type Item = (MachineBasicBlockId, &'a MachineBasicBlock);

    fn next(&mut self) -> Option<Self::Item> {
        self.nth += 1;
        let id = *self.basic_blocks.order.get(self.nth - 1)?;
        Some((id, &self.basic_blocks.arena[id]))
    }
}

impl MachineBasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: RefCell::new(vec![]),
            pred: vec![],
            succ: vec![],
            liveness: Rc::new(RefCell::new(LivenessInfo::new())),
        }
    }

    pub fn iseq_ref(&self) -> Ref<Vec<MachineInstId>> {
        self.iseq.borrow()
    }

    pub fn iseq_ref_mut(&self) -> RefMut<Vec<MachineInstId>> {
        self.iseq.borrow_mut()
    }

    pub fn find_inst_pos(&self, id2find: MachineInstId) -> Option<usize> {
        self.iseq_ref()
            .iter()
            .enumerate()
            .find(|(_, id)| *id == &id2find)
            .map(|(i, _)| i)
    }

    pub fn liveness_ref(&self) -> Ref<LivenessInfo> {
        self.liveness.borrow()
    }

    pub fn liveness_ref_mut(&self) -> RefMut<LivenessInfo> {
        self.liveness.borrow_mut()
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

    pub fn clear(&mut self) {
        self.def.clear();
        self.live_in.clear();
        self.live_out.clear();
    }
}
