pub use crate::codegen::common::dag::node::*;
use crate::codegen::common::new_dag::{node, node::NodeId};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum MemNodeKind {
    BaseFi,
    BaseFiOff,
    BaseFiAlignOff,
    BaseFiAlignOffOff,
    BaseAlignOff,
    BaseOff,
    Base,
    Address,
    AddressOff,
    AddressAlignOff,
}

#[derive(Clone, PartialEq)]
pub enum MemKind {
    // BaseFi(NodeId, NodeId),
    BaseFi(Vec<NodeId>),
    // BaseFiOff(NodeId, NodeId, NodeId),
    BaseFiOff(Vec<NodeId>),
    // BaseFiAlignOff(NodeId, NodeId, NodeId, NodeId),
    BaseFiAlignOff(Vec<NodeId>),
    // BaseFiAlignOffOff,
    BaseAlignOff([NodeId; 3]),
    // BaseOff(NodeId, NodeId),
    BaseOff(Vec<NodeId>),
    Base(NodeId),
    // Address,
    // AddressOff,
    // AddressAlignOff,
}

impl MemKind {
    pub fn args(&self) -> &[NodeId] {
        match self {
            Self::BaseFi(args)
            | Self::BaseFiOff(args)
            | Self::BaseFiAlignOff(args)
            | Self::BaseOff(args) => args,
            Self::BaseAlignOff(args) => args,
            Self::Base(arg) => ::core::slice::from_ref(arg),
        }
    }

    pub fn args_mut(&mut self) -> &mut [NodeId] {
        match self {
            Self::BaseFi(args)
            | Self::BaseFiOff(args)
            | Self::BaseFiAlignOff(args)
            | Self::BaseOff(args) => args,
            Self::BaseAlignOff(args) => args,
            Self::Base(arg) => ::core::slice::from_mut(arg),
        }
    }
}

impl fmt::Debug for MemKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::BaseFi(_) => "BaseFi",
                Self::BaseFiOff(_) => "BaseFiOff",
                Self::BaseFiAlignOff(_) => "BaseFiAlignOff",
                Self::BaseAlignOff(_) => "BaseAlignOff",
                Self::BaseOff(_) => "BaseOff",
                Self::Base(_) => "Base",
            }
        )
    }
}
