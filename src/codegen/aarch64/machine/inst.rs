use super::super::machine::register::RegisterId;
use super::frame_object::*;
pub use super::inst_def::TargetOpcode;
pub use crate::codegen::common::machine::inst::*;
use crate::ir::types::Type;

#[derive(Debug, Clone)]
pub enum MachineMemOperand {
    RegFi(RegisterId, FrameIndexInfo),
    PreIndex(RegisterId, i32),
    PostIndex(RegisterId, i32),
    // ImmReg(i32, RegisterId),
    Address(AddressKind),
}

impl MachineOpcode {
    pub fn is_copy_like(&self) -> bool {
        matches!(self, MachineOpcode::Copy | MachineOpcode::MOVrr)
    }

    pub fn is_terminator(&self) -> bool {
        matches!(self, MachineOpcode::B | MachineOpcode::BEQ)
    }

    pub fn is_unconditional_jmp(&self) -> bool {
        matches!(self, MachineOpcode::B)
    }
}

impl MachineMemOperand {
    pub fn registers(&self) -> Vec<&RegisterId> {
        match self {
            Self::PostIndex(r, _) | Self::PreIndex(r, _) | Self::RegFi(r, _) => vec![r],
            Self::Address(_) => vec![],
        }
    }

    pub fn registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match self {
            Self::PostIndex(r, _) | Self::PreIndex(r, _) | Self::RegFi(r, _) => vec![r],
            Self::Address(_) => vec![],
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            Self::RegFi(_, fi) => Some(fi.ty),
            Self::PostIndex(_, _) | Self::PreIndex(_, _) | Self::Address(_) => None,
        }
    }
}
