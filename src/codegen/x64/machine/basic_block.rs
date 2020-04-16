// use super::{module::*, opcode::*, value::*};
use super::super::register::{PhysRegSet, TargetRegisterTrait, VirtReg};
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
    pub phys_def: PhysRegSet,
    pub def: FxHashSet<VirtReg>,
    pub live_in: FxHashSet<VirtReg>,
    pub live_out: FxHashSet<VirtReg>,
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

    pub fn get_def_phys_regs(&self) -> PhysRegSet {
        let mut set = PhysRegSet::new();
        for (_id, block) in self.id_and_block() {
            set = set | block.liveness_ref().phys_def.clone();
        }
        set
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
            phys_def: PhysRegSet::new(),
            def: FxHashSet::default(),
            live_in: FxHashSet::default(),
            live_out: FxHashSet::default(),
        }
    }

    pub fn add_phys_def<T: TargetRegisterTrait>(&mut self, r: T) {
        self.phys_def |= r.regs_sharing_same_register_file();
    }

    pub fn clear(&mut self) {
        self.phys_def = PhysRegSet::new();
        self.def.clear();
        self.live_in.clear();
        self.live_out.clear();
    }
}
