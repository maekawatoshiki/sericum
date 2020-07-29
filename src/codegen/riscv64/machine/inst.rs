use super::super::machine::register::RegisterId;
use super::frame_object::*;
pub use super::inst_def::TargetOpcode;
pub use crate::codegen::common::machine::{basic_block::MachineBasicBlockId, inst::*};
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
            MachineOpcode::Ret | MachineOpcode::JR | MachineOpcode::J | MachineOpcode::BEQ
        )
    }

    pub fn is_unconditional_jmp(&self) -> bool {
        matches!(self, MachineOpcode::JR | MachineOpcode::J)
    }

    pub fn is_conditional_jmp(&self) -> bool {
        matches!(
            self,
            MachineOpcode::BEQ | MachineOpcode::BLT | MachineOpcode::BLE
        )
    }

    pub fn flip_conditional_jmp(&self) -> Option<Self> {
        match self {
            Self::BEQ => Some(Self::BNE),
            Self::BNE => Some(Self::BEQ),
            Self::BGE => Some(Self::BLT),
            Self::BGT => Some(Self::BLE),
            Self::BLE => Some(Self::BGT),
            Self::BLT => Some(Self::BGE),
            _ => None,
        }
    }
}

impl MachineInst {
    pub fn get_jmp_dst(&self) -> Option<MachineBasicBlockId> {
        if !self.opcode.is_jmp() {
            return None;
        }

        match self.opcode {
            MachineOpcode::J => Some(self.operand[0].as_basic_block()),
            MachineOpcode::BEQ
            | MachineOpcode::BNE
            | MachineOpcode::BGE
            | MachineOpcode::BGT
            | MachineOpcode::BLE
            | MachineOpcode::BLT => Some(self.operand[2].as_basic_block()),
            _ => None,
        }
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
