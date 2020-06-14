pub use crate::codegen::common::dag::node::*;

#[derive(Debug, Clone, PartialEq)]
pub enum MemNodeKind {
    BaseFi,
    BaseFiOff,
    BaseFiAlignOff,
    BaseAlignOff,
    Base,
}
