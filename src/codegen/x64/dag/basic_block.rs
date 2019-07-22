use super::node::DAGNodeId;
use id_arena::*;
use rustc_hash::FxHashSet;
use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

pub type DAGBasicBlockId = Id<DAGBasicBlock>;

#[derive(Clone, Debug)]
pub struct DAGBasicBlock {
    /// Information for liveness analysis
    pub liveness: Rc<RefCell<LivenessInfo>>,

    /// Predecessors
    pub pred: Vec<DAGBasicBlockId>,

    /// Successors
    pub succ: Vec<DAGBasicBlockId>,

    /// Entry node
    pub entry: Option<DAGNodeId>,
}

#[derive(Clone, Debug)]
pub struct LivenessInfo {
    pub def: FxHashSet<DAGNodeId>,
    pub live_in: FxHashSet<DAGNodeId>,
    pub live_out: FxHashSet<DAGNodeId>,
}

impl DAGBasicBlock {
    pub fn new() -> Self {
        Self {
            entry: None,
            pred: vec![],
            succ: vec![],
            liveness: Rc::new(RefCell::new(LivenessInfo::new())),
        }
    }

    pub fn set_entry(&mut self, entry: DAGNodeId) {
        self.entry = Some(entry);
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
