use super::super::machine::register::RegisterId;
use super::frame_object::*;
pub use super::inst_def::TargetOpcode;
pub use crate::codegen::common::machine::inst::*;
use crate::ir::types::Type;

#[derive(Debug, Clone)]
pub enum MachineMemOperand {
    FiReg(FrameIndexInfo, RegisterId),
    ImmReg(i32, RegisterId),
    Address(AddressKind),
}

impl MachineOpcode {
    pub fn is_copy_like(&self) -> bool {
        matches!(self, MachineOpcode::MV | MachineOpcode::Copy)
    }

    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            MachineOpcode::JR | MachineOpcode::J | MachineOpcode::BEQ
        )
    }

    pub fn is_unconditional_jmp(&self) -> bool {
        matches!(self, MachineOpcode::JR | MachineOpcode::J)
    }
}

impl MachineMemOperand {
    pub fn registers(&self) -> Vec<&RegisterId> {
        match self {
            MachineMemOperand::ImmReg(_, r) | MachineMemOperand::FiReg(_, r) => vec![r],
            MachineMemOperand::Address(_) => vec![],
        }
    }

    pub fn registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match self {
            MachineMemOperand::ImmReg(_, r) | MachineMemOperand::FiReg(_, r) => vec![r],
            MachineMemOperand::Address(_) => vec![],
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            Self::FiReg(fi, _) => Some(fi.ty),
            Self::ImmReg(_, _) | Self::Address(_) => None,
        }
    }
}
