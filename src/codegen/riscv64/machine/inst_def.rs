use crate::codegen::arch::machine::register::*;
use crate::codegen::common::machine::inst_def::*;

#[allow(non_upper_case_globals)]
mod inst {
    use super::*;

    // TODO: need macro to describe the followings
    lazy_static! {
        pub static ref ADDI: TargetInstDef = TargetInstDef::new("addi", TargetOpcode::ADDI)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Immediate(TargetImmediate::I32),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref ADDIW: TargetInstDef = TargetInstDef::new("addiw", TargetOpcode::ADDIW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Immediate(TargetImmediate::I32),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref ADDW: TargetInstDef = TargetInstDef::new("addw", TargetOpcode::ADDW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref ADD: TargetInstDef = TargetInstDef::new("add", TargetOpcode::ADD)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref MUL: TargetInstDef = TargetInstDef::new("mul", TargetOpcode::MUL)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref MULW: TargetInstDef = TargetInstDef::new("mulw", TargetOpcode::MULW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref DIVW: TargetInstDef = TargetInstDef::new("divw", TargetOpcode::DIVW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref REMW: TargetInstDef = TargetInstDef::new("remw", TargetOpcode::REMW)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref SLLI: TargetInstDef = TargetInstDef::new("slli", TargetOpcode::SLLI)
            .set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Immediate(TargetImmediate::I8)
            ])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref MV: TargetInstDef = TargetInstDef::new("mv", TargetOpcode::LI)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
                RegisterClassKind::GPR
            ))])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref LA: TargetInstDef = TargetInstDef::new("la", TargetOpcode::LA)
            .set_uses(vec![TargetOperand::Mem])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref LI: TargetInstDef = TargetInstDef::new("li", TargetOpcode::LI)
            .set_uses(vec![TargetOperand::Any])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref LW: TargetInstDef = TargetInstDef::new("lw", TargetOpcode::LW)
            .set_uses(vec![TargetOperand::Mem])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref LD: TargetInstDef = TargetInstDef::new("ld", TargetOpcode::LD)
            .set_uses(vec![TargetOperand::Mem])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref SW: TargetInstDef =
            TargetInstDef::new("sw", TargetOpcode::SW).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Mem
            ]);
        pub static ref SD: TargetInstDef =
            TargetInstDef::new("sd", TargetOpcode::SD).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Mem
            ]);
        pub static ref SEXT_W: TargetInstDef = TargetInstDef::new("sext.w", TargetOpcode::SEXT_W)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
                RegisterClassKind::GPR
            )),])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref CALL: TargetInstDef = TargetInstDef::new("call", TargetOpcode::CALL);
        pub static ref BEQ: TargetInstDef =
            TargetInstDef::new("beq", TargetOpcode::BEQ).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Block
            ]);
        pub static ref BNE: TargetInstDef =
            TargetInstDef::new("bne", TargetOpcode::BNE).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Block
            ]);
        pub static ref BLE: TargetInstDef =
            TargetInstDef::new("ble", TargetOpcode::BLE).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Block
            ]);
        pub static ref BLT: TargetInstDef =
            TargetInstDef::new("blt", TargetOpcode::BLT).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Block
            ]);
        pub static ref BGE: TargetInstDef =
            TargetInstDef::new("bge", TargetOpcode::BGE).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Block
            ]);
        pub static ref BGT: TargetInstDef =
            TargetInstDef::new("bgt", TargetOpcode::BGT).set_uses(vec![
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
                TargetOperand::Block
            ]);
        pub static ref J: TargetInstDef =
            TargetInstDef::new("j", TargetOpcode::J).set_uses(vec![TargetOperand::Block]);
        pub static ref JR: TargetInstDef =
            TargetInstDef::new("jr", TargetOpcode::JR).set_uses(vec![TargetOperand::Register(
                TargetRegister::RegClass(RegisterClassKind::GPR)
            )]);
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[allow(non_camel_case_types)]
pub enum TargetOpcode {
    // CALL,

    // Comparison
    Seteq,
    Setle,
    Setlt,

    ADDI,  // Add Integer
    ADDIW, // Add Integer Word
    ADDW,  // Add Word
    ADD,   // Add
    MUL,   // Mul
    MULW,  // Mul Word
    DIVW,  // Div Word
    REMW,  // Rem Word
    SLLI,  // Shift Left Logical Immediate
    MV,    // Move
    LA,
    LI,     // Load Immediate
    LW,     // Load Word
    LD,     // Load Double
    SW,     // Store Word
    SD,     // Store Double
    SEXT_W, // Sign-extend Word
    CALL,
    BEQ,
    BNE,
    BLE,
    BLT,
    BGE,
    BGT,
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
    pub fn inst_def(&self) -> Option<&TargetInstDef> {
        match self {
            Self::ADDI => Some(&*inst::ADDI),
            Self::ADDIW => Some(&*inst::ADDIW),
            Self::ADDW => Some(&*inst::ADDW),
            Self::ADD => Some(&*inst::ADD),
            Self::MULW => Some(&*inst::MULW),
            Self::MUL => Some(&*inst::MUL),
            Self::DIVW => Some(&*inst::DIVW),
            Self::REMW => Some(&*inst::REMW),
            Self::SLLI => Some(&*inst::SLLI),
            Self::MV => Some(&*inst::MV),
            Self::LA => Some(&*inst::LA),
            Self::LI => Some(&*inst::LI),
            Self::LW => Some(&*inst::LW),
            Self::LD => Some(&*inst::LD),
            Self::SW => Some(&*inst::SW),
            Self::SD => Some(&*inst::SD),
            Self::SEXT_W => Some(&*inst::SEXT_W),
            Self::CALL => Some(&*inst::CALL),
            Self::BEQ => Some(&*inst::BEQ),
            Self::BNE => Some(&*inst::BNE),
            Self::BLE => Some(&*inst::BLE),
            Self::BLT => Some(&*inst::BLT),
            Self::BGE => Some(&*inst::BGE),
            Self::BGT => Some(&*inst::BGT),
            Self::J => Some(&*inst::J),
            Self::JR => Some(&*inst::JR),
            _ => None,
        }
    }
}
