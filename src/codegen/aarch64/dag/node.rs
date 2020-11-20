pub use crate::codegen::common::dag::node::*;
use crate::codegen::common::new_dag::node::NodeId;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum MemNodeKind {
    // RegImm,
    RegFi,
    Reg,
    // FiReg,
    // ImmReg,
    Address,
}

#[derive(Clone, PartialEq)]
pub enum MemKind {
    // RegImm,
    RegFi([NodeId; 2]),
    Reg(NodeId),
    // FiReg,
    // ImmReg,
    Address(NodeId),
}

impl MemKind {
    pub fn args(&self) -> &[NodeId] {
        match self {
            Self::RegFi(args) => args,
            Self::Reg(arg) | Self::Address(arg) => ::core::slice::from_ref(arg),
        }
    }

    pub fn args_mut(&mut self) -> &mut [NodeId] {
        match self {
            Self::RegFi(args) => args,
            Self::Reg(arg) | Self::Address(arg) => ::core::slice::from_mut(arg),
        }
    }
}

impl fmt::Debug for MemKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::RegFi(_) => "RegFi",
                Self::Reg(_) => "Reg",
                Self::Address(_) => "Address",
            }
        )
    }
}
