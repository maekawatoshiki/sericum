// use super::{module::*, opcode::*, value::*};
use super::instr::*;
use id_arena::*;
use rustc_hash::FxHashSet;
use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

pub type MachineBasicBlockId = Id<MachineBasicBlock>;

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
    pub iseq: Rc<RefCell<Vec<MachineInstrId>>>,
}

#[derive(Clone, Debug)]
pub struct LivenessInfo {
    pub def: FxHashSet<MachineRegister>,
    pub live_in: FxHashSet<MachineRegister>,
    pub live_out: FxHashSet<MachineRegister>,
}

impl MachineBasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: Rc::new(RefCell::new(vec![])),
            pred: vec![],
            succ: vec![],
            liveness: Rc::new(RefCell::new(LivenessInfo::new())),
        }
    }

    pub fn iseq_ref(&self) -> Ref<Vec<MachineInstrId>> {
        self.iseq.borrow()
    }

    pub fn iseq_ref_mut(&self) -> RefMut<Vec<MachineInstrId>> {
        self.iseq.borrow_mut()
    }

    pub fn find_instr_pos(&self, id2find: MachineInstrId) -> Option<usize> {
        self.iseq_ref()
            .iter()
            .enumerate()
            .find(|(_, id)| *id == &id2find)
            .map(|(i, _)| i)
    }

    pub fn liveness_ref(&self) -> Ref<LivenessInfo> {
        self.liveness.borrow()
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
