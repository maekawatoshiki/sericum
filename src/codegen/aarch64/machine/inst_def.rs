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
        pub static ref ADDrrr64: TargetInstDef = TargetInstDef::new("add", TargetOpcode::ADDrrr64)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                           TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64))])
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
        pub static ref MULrrr64: TargetInstDef = TargetInstDef::new("mul", TargetOpcode::MULrrr64)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                           TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64))])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)]);
        pub static ref SDIVrrr32: TargetInstDef = TargetInstDef::new("sdiv", TargetOpcode::SDIVrrr32)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                           TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32))])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref CMPri: TargetInstDef = TargetInstDef::new("cmp", TargetOpcode::CMPri)
            .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                           TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32))]);
        pub static ref B_EQ: TargetInstDef = TargetInstDef::new("b.eq", TargetOpcode::B_EQ).set_uses(vec![TargetOperand::Block]);
        pub static ref B_NE: TargetInstDef = TargetInstDef::new("b.ne", TargetOpcode::B_NE).set_uses(vec![TargetOperand::Block]);
        pub static ref B_LT: TargetInstDef = TargetInstDef::new("b.lt", TargetOpcode::B_LT).set_uses(vec![TargetOperand::Block]);
        pub static ref B_LE: TargetInstDef = TargetInstDef::new("b.le", TargetOpcode::B_LE).set_uses(vec![TargetOperand::Block]);
        pub static ref B_GT: TargetInstDef = TargetInstDef::new("b.gt", TargetOpcode::B_GT).set_uses(vec![TargetOperand::Block]);
        pub static ref B_GE: TargetInstDef = TargetInstDef::new("b.ge", TargetOpcode::B_GE).set_uses(vec![TargetOperand::Block]);
        pub static ref B: TargetInstDef = TargetInstDef::new("b", TargetOpcode::B).set_uses(vec![TargetOperand::Block]);
        pub static ref BL: TargetInstDef = TargetInstDef::new("bl", TargetOpcode::CALL).set_uses(vec![TargetOperand::Addr]);
        pub static ref LDR32: TargetInstDef = TargetInstDef::new("ldr", TargetOpcode::LDR32)
            .set_uses(vec![TargetOperand::Mem])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)]);
        pub static ref LDR64: TargetInstDef = TargetInstDef::new("ldr", TargetOpcode::LDR64)
            .set_uses(vec![TargetOperand::Mem])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)]);
        pub static ref LDRSW64: TargetInstDef = TargetInstDef::new("ldrsw", TargetOpcode::LDRSW64)
            .set_uses(vec![TargetOperand::Mem])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)]);
        pub static ref STR: TargetInstDef = TargetInstDef::new("str", TargetOpcode::STR)
            .set_uses(vec![TargetOperand::Register(TargetRegister::Any), TargetOperand::Mem]);
        pub static ref STP: TargetInstDef = TargetInstDef::new("stp", TargetOpcode::STP)
            .set_uses(vec![TargetOperand::Register(TargetRegister::Any),
                           TargetOperand::Register(TargetRegister::Any),
                           TargetOperand::Mem]);
        pub static ref LDP64: TargetInstDef = TargetInstDef::new("ldp", TargetOpcode::LDP64)
            .set_uses(vec![TargetOperand::Mem])
            .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64),
                           TargetRegister::RegClass(RegisterClassKind::GR64)]);
        pub static ref RET: TargetInstDef = TargetInstDef::new("ret", TargetOpcode::RET);
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
    ADDrrr64,
    SUBrr64i,
    SUBrr32i,
    SUBrrr32,
    MULrrr32,
    MULrrr64,
    SDIVrrr32,
    CMPri,
    B_EQ,
    B_NE,
    B_LT,
    B_LE,
    B_GT,
    B_GE,
    B,
    LDR32,
    LDR64,
    LDRSW64,
    STR,
    LDP64,
    STP,
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
            Self::ADDrrr64 => Some(&*inst::ADDrrr64),
            Self::SUBrr64i => Some(&*inst::SUBrr64i),
            Self::SUBrr32i => Some(&*inst::SUBrr32i),
            Self::SUBrrr32 => Some(&*inst::SUBrrr32),
            Self::MULrrr32 => Some(&*inst::MULrrr32),
            Self::MULrrr64 => Some(&*inst::MULrrr64),
            Self::SDIVrrr32 => Some(&*inst::SDIVrrr32),
            Self::CMPri => Some(&*inst::CMPri),
            Self::B_EQ => Some(&*inst::B_EQ),
            Self::B_NE => Some(&*inst::B_NE),
            Self::B_LT => Some(&*inst::B_LT),
            Self::B_LE => Some(&*inst::B_LE),
            Self::B_GT => Some(&*inst::B_GT),
            Self::B_GE => Some(&*inst::B_GE),
            Self::B => Some(&*inst::B),
            Self::CALL => Some(&*inst::BL),
            Self::LDR32 => Some(&*inst::LDR32),
            Self::LDR64 => Some(&*inst::LDR64),
            Self::LDRSW64 => Some(&*inst::LDRSW64),
            Self::STR => Some(&*inst::STR),
            Self::LDP64 => Some(&*inst::LDP64),
            Self::STP => Some(&*inst::STP),
            Self::RET => Some(&*inst::RET),
            _ => None,
        }
    }
}
