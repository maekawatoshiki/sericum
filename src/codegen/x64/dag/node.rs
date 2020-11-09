pub use crate::codegen::common::dag::node::*;
use crate::codegen::common::new_dag::node::NodeId;

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

#[derive(Debug, Clone, PartialEq)]
pub enum MemKind {
    BaseFi(NodeId, NodeId),
    BaseFiOff(NodeId, NodeId, NodeId),
    BaseFiAlignOff(NodeId, NodeId, NodeId, NodeId),
    // BaseFiAlignOffOff,
    // BaseAlignOff,
    // BaseOff,
    Base(NodeId),
    // Address,
    // AddressOff,
    // AddressAlignOff,
}
