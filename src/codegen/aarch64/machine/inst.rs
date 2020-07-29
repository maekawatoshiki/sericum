use super::super::machine::register::RegisterId;
use super::frame_object::*;
pub use super::inst_def::TargetOpcode;
pub use crate::codegen::common::machine::{basic_block::MachineBasicBlockId, inst::*};
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
        matches!(
            self,
            MachineOpcode::RET
                | MachineOpcode::Ret
                | MachineOpcode::B
                | MachineOpcode::B_EQ
                | MachineOpcode::B_NE
                | MachineOpcode::B_LE
                | MachineOpcode::B_LT
                | MachineOpcode::B_GE
                | MachineOpcode::B_GT
        )
    }

    pub fn is_unconditional_jmp(&self) -> bool {
        matches!(self, MachineOpcode::B)
    }

    pub fn is_conditional_jmp(&self) -> bool {
        matches!(
            self,
            MachineOpcode::B_EQ
                | MachineOpcode::B_NE
                | MachineOpcode::B_LE
                | MachineOpcode::B_LT
                | MachineOpcode::B_GE
                | MachineOpcode::B_GT
        )
    }

    pub fn is_jmp(&self) -> bool {
        self.is_unconditional_jmp() | self.is_conditional_jmp()
    }

    pub fn flip_conditional_jmp(&self) -> Option<Self> {
        match self {
            Self::B_EQ => Some(Self::B_NE),
            Self::B_NE => Some(Self::B_EQ),
            Self::B_GE => Some(Self::B_LT),
            Self::B_GT => Some(Self::B_LE),
            Self::B_LE => Some(Self::B_GT),
            Self::B_LT => Some(Self::B_GE),
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
            MachineOpcode::B
            | MachineOpcode::B_EQ
            | MachineOpcode::B_NE
            | MachineOpcode::B_GE
            | MachineOpcode::B_GT
            | MachineOpcode::B_LE
            | MachineOpcode::B_LT => Some(self.operand[0].as_basic_block()),
            _ => None,
        }
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
