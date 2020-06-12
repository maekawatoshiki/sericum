use super::register::*;
use crate::codegen::common::machine::inst_def::*;

type TargetInstDefTy = TargetInstDef<TargetOpcode, TargetRegister<RegisterClassKind, PhysReg>>;

#[allow(non_upper_case_globals)]
mod inst {
    use super::*;

    // TODO: need macro to describe the followings
    lazy_static! {
        pub static ref ADDI: TargetInstDefTy = TargetInstDef::new("addi", TargetOpcode::ADDI)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Immediate(TargetImmediate::I32),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref ADDIW: TargetInstDefTy = TargetInstDef::new("addiw", TargetOpcode::ADDIW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Immediate(TargetImmediate::I32),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref ADDW: TargetInstDefTy = TargetInstDef::new("addw", TargetOpcode::ADDW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref MULW: TargetInstDefTy = TargetInstDef::new("mulw", TargetOpcode::MULW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref DIVW: TargetInstDefTy = TargetInstDef::new("divw", TargetOpcode::DIVW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref MV: TargetInstDefTy = TargetInstDef::new("mv", TargetOpcode::LI)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
                RegisterClassKind::GPR
            ))])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref LI: TargetInstDefTy = TargetInstDef::new("li", TargetOpcode::LI)
            .set_uses(vec![TargetOperand::Any])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref LW: TargetInstDefTy = TargetInstDef::new("lw", TargetOpcode::LW)
            .set_uses(vec![
                TargetOperand::Immediate(TargetImmediate::I32),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR))
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref LD: TargetInstDefTy = TargetInstDef::new("ld", TargetOpcode::LD)
            .set_uses(vec![
                TargetOperand::Immediate(TargetImmediate::I32),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR))
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref SW: TargetInstDefTy =
            TargetInstDef::new("sw", TargetOpcode::SW).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Immediate(TargetImmediate::I32),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR))
            ]);
        pub static ref SD: TargetInstDefTy =
            TargetInstDef::new("sd", TargetOpcode::SD).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Immediate(TargetImmediate::I32),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR))
            ]);
        pub static ref J: TargetInstDefTy =
            TargetInstDef::new("j", TargetOpcode::J).set_uses(vec![TargetOperand::Block]);
        pub static ref JR: TargetInstDefTy =
            TargetInstDef::new("jr", TargetOpcode::JR).set_uses(vec![TargetOperand::Register(
                TargetRegister::RegClass(RegisterClassKind::GPR)
            )]);
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TargetOpcode {
    // CALL,

    // Comparison
    Seteq,
    Setle,
    Setlt,

    ADDI,  // Add Integer
    ADDIW, // Add Integer Word
    ADDW,  // Add Word
    MULW,  // Mul Word
    DIVW,  // Div Word
    MV,    // Move
    LI,    // Load Immediate
    LW,    // Load Word
    LD,    // Load Double
    SW,    // Store Word
    SD,    // Store Double
    // BrccEq,
    // BrccLe,
    // BrccLt,
    // CMPrr,
    // CMPri,
    // UCOMISDrr,
    // JE,
    // JBE,
    // JB,
    // JLE,
    // JL,
    // JA,
    // JAE,
    // JG,
    // JGE,
    // JMP,
    J,
    JR,

    Phi,
    Ret,
    Copy,
    BrCond,
    AdjStackDown,
    AdjStackUp,
}

impl TargetOpcode {
    pub fn inst_def(&self) -> Option<&TargetInstDefTy> {
        match self {
            Self::ADDI => Some(&*inst::ADDI),
            Self::ADDIW => Some(&*inst::ADDIW),
            Self::ADDW => Some(&*inst::ADDW),
            Self::MULW => Some(&*inst::MULW),
            Self::DIVW => Some(&*inst::DIVW),
            Self::MV => Some(&*inst::MV),
            Self::LI => Some(&*inst::LI),
            Self::LW => Some(&*inst::LW),
            Self::LD => Some(&*inst::LD),
            Self::SW => Some(&*inst::SW),
            Self::SD => Some(&*inst::SD),
            Self::J => Some(&*inst::J),
            Self::JR => Some(&*inst::JR),
            _ => None,
        }
    }
}
