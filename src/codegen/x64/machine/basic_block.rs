// use super::{module::*, opcode::*, value::*};
use super::instr::*;
use id_arena::*;
use rustc_hash::FxHashSet;
use std::{cell::RefCell, rc::Rc};

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
    pub iseq: Vec<MachineInstrId>,
}

#[derive(Clone, Debug)]
pub struct LivenessInfo {
    pub def: FxHashSet<MachineInstrId>,
    pub live_in: FxHashSet<MachineInstrId>,
    pub live_out: FxHashSet<MachineInstrId>,
}

impl MachineBasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: vec![],
            pred: vec![],
            succ: vec![],
            liveness: Rc::new(RefCell::new(LivenessInfo::new())),
        }
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
