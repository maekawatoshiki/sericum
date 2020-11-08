use crate::codegen::common::dag::node::NodeId;
use crate::ir::types::Types;
use id_arena::*;
use rustc_hash::FxHashSet;
use std::fmt;

pub type DAGBasicBlockId = Id<DAGBasicBlock>;

#[derive(Clone)]
pub struct DAGBasicBlock {
    /// Predecessors
    pub pred: FxHashSet<DAGBasicBlockId>,

    /// Successors
    pub succ: FxHashSet<DAGBasicBlockId>,

    /// Entry node
    pub entry: Option<NodeId>,

    /// Root node
    pub root: Option<NodeId>,
}

impl DAGBasicBlock {
    pub fn new() -> Self {
        Self {
            entry: None,
            root: None,
            pred: FxHashSet::default(),
            succ: FxHashSet::default(),
        }
    }

    pub fn set_entry(&mut self, entry: NodeId) {
        self.entry = Some(entry);
    }

    pub fn set_root(&mut self, root: NodeId) {
        self.root = Some(root);
    }

    pub fn debug(&self, f: &mut fmt::Formatter<'_>, tys: &Types, bb_idx: usize) -> fmt::Result {
        writeln!(
            f,
            "BB({}); pred: {{{}}}, succ: {{{}}});",
            bb_idx,
            self.pred
                .iter()
                .fold("".to_string(), |s, id| format!("{}{},", s, id.index()))
                .trim_matches(','),
            self.succ
                .iter()
                .fold("".to_string(), |s, id| format!("{}{},", s, id.index()))
                .trim_matches(','),
        )?;

        if let Some(entry) = self.entry {
            todo!()
            // entry.debug(f, tys, &mut FxHashMap::default(), 0, 2)?;
        }

        fmt::Result::Ok(())
    }
}
