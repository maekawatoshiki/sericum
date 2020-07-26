use crate::codegen::arch::machine::register::*;
use crate::codegen::common::machine::inst_def::*;

#[allow(non_upper_case_globals)]
mod inst {
    use super::*;

    // TODO: need macro to describe the followings
    lazy_static! {
        // ldr, str
        pub static ref MOVrr: TargetInstDef = TargetInstDef::new("mov", TargetOpcode::MOVrr)
            .set_uses(vec![TargetOperand::Register(TargetRegister::Any)])
            .set_defs(vec![TargetRegister::Any]);
        pub static ref MOVr32i: TargetInstDef = TargetInstDef::new("mov", TargetOpcode::MOVr32i)
            .set_uses(vec![TargetOperand::Immediate(TargetImmediate::I16)])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref ADDrr64i: TargetInstDef = TargetInstDef::new("add", TargetOpcode::ADDrr64i)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                           TargetOperand::Immediate(TargetImmediate::I16)]) // TODO: I12
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)]);
        pub static ref ADDrr32i: TargetInstDef = TargetInstDef::new("add", TargetOpcode::ADDrr32i)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                           TargetOperand::Immediate(TargetImmediate::I16)]) // TODO: I12
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref ADDrrr32: TargetInstDef = TargetInstDef::new("add", TargetOpcode::ADDrrr32)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                           TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32))])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref SUBrr64i: TargetInstDef = TargetInstDef::new("sub", TargetOpcode::SUBrr64i)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                           TargetOperand::Immediate(TargetImmediate::I16)]) // TODO: I12
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)]);
        pub static ref SUBrr32i: TargetInstDef = TargetInstDef::new("sub", TargetOpcode::SUBrr32i)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                           TargetOperand::Immediate(TargetImmediate::I16)]) // TODO: I12
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref SUBrrr32: TargetInstDef = TargetInstDef::new("sub", TargetOpcode::SUBrrr32)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                           TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32))])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref MULrrr32: TargetInstDef = TargetInstDef::new("mul", TargetOpcode::MULrrr32)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                           TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32))])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref SDIVrrr32: TargetInstDef = TargetInstDef::new("sdiv", TargetOpcode::SDIVrrr32)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                           TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32))])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref LDR32: TargetInstDef = TargetInstDef::new("ldr", TargetOpcode::LDR32)
            .set_uses(vec![TargetOperand::Mem])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref STR: TargetInstDef = TargetInstDef::new("str", TargetOpcode::STR)
            .set_uses(vec![TargetOperand::Register(TargetRegister::Any), TargetOperand::Mem]);
        pub static ref RET: TargetInstDef = TargetInstDef::new("ret", TargetOpcode::RET);
        // pub static ref ADDI: TargetInstDef = TargetInstDef::new("addi", TargetOpcode::ADDI)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Immediate(TargetImmediate::I32),
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref ADDIW: TargetInstDef = TargetInstDef::new("addiw", TargetOpcode::ADDIW)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Immediate(TargetImmediate::I32),
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref ADDW: TargetInstDef = TargetInstDef::new("addw", TargetOpcode::ADDW)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref ADD: TargetInstDef = TargetInstDef::new("add", TargetOpcode::ADD)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref MUL: TargetInstDef = TargetInstDef::new("mul", TargetOpcode::MUL)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref MULW: TargetInstDef = TargetInstDef::new("mulw", TargetOpcode::MULW)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref DIVW: TargetInstDef = TargetInstDef::new("divw", TargetOpcode::DIVW)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref REMW: TargetInstDef = TargetInstDef::new("remw", TargetOpcode::REMW)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref SLLI: TargetInstDef = TargetInstDef::new("slli", TargetOpcode::SLLI)
        //     .set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Immediate(TargetImmediate::I8)
        //     ])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref MV: TargetInstDef = TargetInstDef::new("mv", TargetOpcode::LI)
        //     .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
        //         RegisterClassKind::GPR
        //     ))])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref LA: TargetInstDef = TargetInstDef::new("la", TargetOpcode::LA)
        //     .set_uses(vec![TargetOperand::Mem])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref LI: TargetInstDef = TargetInstDef::new("li", TargetOpcode::LI)
        //     .set_uses(vec![TargetOperand::Any])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref LW: TargetInstDef = TargetInstDef::new("lw", TargetOpcode::LW)
        //     .set_uses(vec![TargetOperand::Mem])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref LD: TargetInstDef = TargetInstDef::new("ld", TargetOpcode::LD)
        //     .set_uses(vec![TargetOperand::Mem])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref SW: TargetInstDef =
        //     TargetInstDef::new("sw", TargetOpcode::SW).set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Mem
        //     ]);
        // pub static ref SD: TargetInstDef =
        //     TargetInstDef::new("sd", TargetOpcode::SD).set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Mem
        //     ]);
        // pub static ref SEXT_W: TargetInstDef = TargetInstDef::new("sext.w", TargetOpcode::SEXT_W)
        //     .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
        //         RegisterClassKind::GPR
        //     )),])
        //     .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        // pub static ref CALL: TargetInstDef = TargetInstDef::new("call", TargetOpcode::CALL);
        // pub static ref BEQ: TargetInstDef =
        //     TargetInstDef::new("beq", TargetOpcode::BEQ).set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Block
        //     ]);
        // pub static ref BLE: TargetInstDef =
        //     TargetInstDef::new("ble", TargetOpcode::BLE).set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Block
        //     ]);
        // pub static ref BLT: TargetInstDef =
        //     TargetInstDef::new("blt", TargetOpcode::BLT).set_uses(vec![
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR)),
        //         TargetOperand::Block
        //     ]);
        // pub static ref J: TargetInstDef =
        //     TargetInstDef::new("j", TargetOpcode::J).set_uses(vec![TargetOperand::Block]);
        // pub static ref JR: TargetInstDef =
        //     TargetInstDef::new("jr", TargetOpcode::JR).set_uses(vec![TargetOperand::Register(
        //         TargetRegister::RegClass(RegisterClassKind::GPR)
        //     )]);
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[allow(non_camel_case_types)]
pub enum TargetOpcode {
    CALL,
    MOVrr,
    MOVr32i,
    ADDrr64i,
    ADDrr32i,
    ADDrrr32,
    SUBrr64i,
    SUBrr32i,
    SUBrrr32,
    MULrrr32,
    SDIVrrr32,
    LDR32,
    STR,
    RET,
    // ADDI,  // Add Integer
    // ADDIW, // Add Integer Word
    // ADDW,  // Add Word
    // ADD,   // Add
    // MUL,   // Mul
    // MULW,  // Mul Word
    // DIVW,  // Div Word
    // REMW,  // Rem Word
    // SLLI,  // Shift Left Logical Immediate
    // MV,    // Move
    // LA,
    // LI,     // Load Immediate
    // LW,     // Load Word
    // LD,     // Load Double
    // SW,     // Store Word
    // SD,     // Store Double
    // SEXT_W, // Sign-extend Word
    // CALL,
    // BEQ,
    // BLE,
    // BLT,
    // J,
    // JR,
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
            Self::MOVrr => Some(&*inst::MOVrr),
            Self::MOVr32i => Some(&*inst::MOVr32i),
            Self::ADDrr64i => Some(&*inst::ADDrr64i),
            Self::ADDrr32i => Some(&*inst::ADDrr32i),
            Self::ADDrrr32 => Some(&*inst::ADDrrr32),
            Self::SUBrr64i => Some(&*inst::SUBrr64i),
            Self::SUBrr32i => Some(&*inst::SUBrr32i),
            Self::SUBrrr32 => Some(&*inst::SUBrrr32),
            Self::MULrrr32 => Some(&*inst::MULrrr32),
            Self::SDIVrrr32 => Some(&*inst::SDIVrrr32),
            Self::LDR32 => Some(&*inst::LDR32),
            Self::STR => Some(&*inst::STR),
            Self::RET => Some(&*inst::RET),
            // Self::ADDI => Some(&*inst::ADDI),
            // Self::ADDIW => Some(&*inst::ADDIW),
            // Self::ADDW => Some(&*inst::ADDW),
            // Self::ADD => Some(&*inst::ADD),
            // Self::MULW => Some(&*inst::MULW),
            // Self::MUL => Some(&*inst::MUL),
            // Self::DIVW => Some(&*inst::DIVW),
            // Self::REMW => Some(&*inst::REMW),
            // Self::SLLI => Some(&*inst::SLLI),
            // Self::MV => Some(&*inst::MV),
            // Self::LA => Some(&*inst::LA),
            // Self::LI => Some(&*inst::LI),
            // Self::LW => Some(&*inst::LW),
            // Self::LD => Some(&*inst::LD),
            // Self::SW => Some(&*inst::SW),
            // Self::SD => Some(&*inst::SD),
            // Self::SEXT_W => Some(&*inst::SEXT_W),
            // Self::CALL => Some(&*inst::CALL),
            // Self::BEQ => Some(&*inst::BEQ),
            // Self::BLE => Some(&*inst::BLE),
            // Self::BLT => Some(&*inst::BLT),
            // Self::J => Some(&*inst::J),
            // Self::JR => Some(&*inst::JR),
            _ => None,
        }
    }
}
