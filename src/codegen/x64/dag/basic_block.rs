use super::node::DAGNodeId;
use id_arena::*;

pub type DAGBasicBlockId = Id<DAGBasicBlock>;

#[derive(Clone, Debug)]
pub struct DAGBasicBlock {
    /// Predecessors
    pub pred: Vec<DAGBasicBlockId>,

    /// Successors
    pub succ: Vec<DAGBasicBlockId>,

    /// Entry node
    pub entry: Option<DAGNodeId>,
}

impl DAGBasicBlock {
    pub fn new() -> Self {
        Self {
            entry: None,
            pred: vec![],
            succ: vec![],
        }
    }

    pub fn set_entry(&mut self, entry: DAGNodeId) {
        self.entry = Some(entry);
    }
}
