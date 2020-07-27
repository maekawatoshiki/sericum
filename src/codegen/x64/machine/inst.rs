use super::super::machine::register::RegisterId;
use super::frame_object::*;
pub use super::inst_def::TargetOpcode;
pub use crate::codegen::common::machine::inst::*;
use crate::ir::types::Type;

#[derive(Debug, Clone)]
pub enum MachineMemOperand {
    BaseFi(RegisterId, FrameIndexInfo),
    BaseFiOff(RegisterId, FrameIndexInfo, i32), // base, fi, off
    BaseFiAlignOff(RegisterId, FrameIndexInfo, i32, RegisterId), // base, fi, align, off
    BaseAlignOff(RegisterId, i32, RegisterId),  // base, align, off
    BaseOff(RegisterId, i32),
    Base(RegisterId),
    Address(AddressKind),
    AddressOff(AddressKind, i32),
    AddressAlignOff(AddressKind, i32, RegisterId),
}

impl MachineOpcode {
    pub fn is_copy_like(&self) -> bool {
        matches!(
            self,
            MachineOpcode::MOVrr32
                | MachineOpcode::MOVrr64
                | MachineOpcode::Copy
                | MachineOpcode::MOVSDrr
        )
    }

    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            MachineOpcode::Ret
                | MachineOpcode::RET
                | MachineOpcode::JMP
                | MachineOpcode::BrCond
                | MachineOpcode::JE
                | MachineOpcode::JNE
                | MachineOpcode::JL
                | MachineOpcode::JLE
                | MachineOpcode::JG
                | MachineOpcode::JGE
                | MachineOpcode::JA
                | MachineOpcode::JAE
                | MachineOpcode::JBE
                | MachineOpcode::JB
        )
    }

    pub fn is_unconditional_jmp(&self) -> bool {
        matches!(self, MachineOpcode::JMP)
    }

    pub fn is_conditional_jmp(&self) -> bool {
        matches!(
            self,
            MachineOpcode::BrCond
                | MachineOpcode::JE
                | MachineOpcode::JNE
                | MachineOpcode::JL
                | MachineOpcode::JLE
                | MachineOpcode::JG
                | MachineOpcode::JGE
                | MachineOpcode::JA
                | MachineOpcode::JAE
                | MachineOpcode::JBE
                | MachineOpcode::JB
        )
    }

    pub fn is_jmp(&self) -> bool {
        self.is_unconditional_jmp() | self.is_conditional_jmp()
    }

    pub fn flip_conditional_jmp(&self) -> Option<Self> {
        match self {
            MachineOpcode::JE => Some(Self::JNE),
            MachineOpcode::JL => Some(Self::JGE),
            MachineOpcode::JLE => Some(Self::JG),
            MachineOpcode::JG => Some(Self::JLE),
            MachineOpcode::JGE => Some(Self::JL),
            MachineOpcode::JA => Some(Self::JBE),
            MachineOpcode::JAE => Some(Self::JB),
            MachineOpcode::JBE => Some(Self::JA),
            MachineOpcode::JB => Some(Self::JAE),
            _ => None,
        }
    }
}

impl MachineMemOperand {
    pub fn registers(&self) -> Vec<&RegisterId> {
        match self {
            MachineMemOperand::BaseFi(r, _)
            | MachineMemOperand::BaseFiOff(r, _, _)
            | MachineMemOperand::BaseOff(r, _)
            | MachineMemOperand::Base(r)
            | Self::AddressAlignOff(_, _, r) => vec![r],
            MachineMemOperand::BaseAlignOff(r, _, r2)
            | MachineMemOperand::BaseFiAlignOff(r, _, _, r2) => vec![r, r2],
            MachineMemOperand::Address(_) | MachineMemOperand::AddressOff(_, _) => vec![],
        }
    }

    pub fn registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match self {
            MachineMemOperand::BaseFi(r, _)
            | MachineMemOperand::BaseFiOff(r, _, _)
            | MachineMemOperand::BaseOff(r, _)
            | MachineMemOperand::Base(r)
            | Self::AddressAlignOff(_, _, r) => vec![r],
            MachineMemOperand::BaseAlignOff(r, _, r2)
            | MachineMemOperand::BaseFiAlignOff(r, _, _, r2) => vec![r, r2],
            MachineMemOperand::Address(_) | MachineMemOperand::AddressOff(_, _) => vec![],
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            Self::BaseFi(_, fi) => Some(fi.ty),
            Self::BaseFiOff(_, _, _) => None,
            Self::BaseFiAlignOff(_, _, _, _) => None,
            Self::Base(_)
            | Self::BaseAlignOff(_, _, _)
            | Self::BaseOff(_, _)
            | Self::Address(_)
            | Self::AddressOff(_, _)
            | Self::AddressAlignOff(_, _, _) => None,
        }
    }
}
