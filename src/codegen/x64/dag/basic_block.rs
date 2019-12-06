use super::node::{DAGNode, DAGNodeId};
use crate::util::allocator::*;
use id_arena::*;

pub type DAGBasicBlockId = Id<DAGBasicBlock>;

#[derive(Clone, Debug)]
pub struct DAGBasicBlock {
    /// Predecessors
    pub pred: Vec<DAGBasicBlockId>,

    /// Successors
    pub succ: Vec<DAGBasicBlockId>,

    /// Entry node
    pub entry: Option<Raw<DAGNode>>,
}

impl DAGBasicBlock {
    pub fn new() -> Self {
        Self {
            entry: None,
            pred: vec![],
            succ: vec![],
        }
    }

    pub fn set_entry(&mut self, entry: Raw<DAGNode>) {
        self.entry = Some(entry);
    }
}
