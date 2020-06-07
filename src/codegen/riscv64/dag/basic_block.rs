use super::node::DAGNode;
use crate::{ir::types::Types, util::allocator::*};
use id_arena::*;
use rustc_hash::FxHashMap;
use std::fmt;

pub type DAGBasicBlockId = Id<DAGBasicBlock>;

#[derive(Clone)]
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
