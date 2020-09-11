// use super::node::DAGNode;
use crate::codegen::arch::dag::node::DAGNode;
use crate::{ir::types::Types, util::allocator::*};
use id_arena::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::fmt;

pub type DAGBasicBlockId = Id<DAGBasicBlock>;

#[derive(Clone)]
pub struct DAGBasicBlock {
    /// Predecessors
    pub pred: FxHashSet<DAGBasicBlockId>,

    /// Successors
    pub succ: FxHashSet<DAGBasicBlockId>,

    /// Entry node
    pub entry: Option<Raw<DAGNode>>,

    /// Root node
    pub root: Option<Raw<DAGNode>>,
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

    pub fn set_entry(&mut self, entry: Raw<DAGNode>) {
        self.entry = Some(entry);
    }

    pub fn set_root(&mut self, root: Raw<DAGNode>) {
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
            entry.debug(f, tys, &mut FxHashMap::default(), 0, 2)?;
        }

        fmt::Result::Ok(())
    }
}
