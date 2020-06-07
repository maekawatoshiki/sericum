use super::register::*;
use rustc_hash::FxHashMap;

#[allow(non_upper_case_globals)]
mod inst {
    use super::*;

    // TODO: need macro to describe the followings
    lazy_static! {
        // pub static ref MOVSDrm64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVSDrm64)
        //         .set_uses(vec![TargetOperand::Mem])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        // };
        // pub static ref MOVSDrr: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVSDrr)
        //         .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
        //             RegisterClassKind::XMM,
        //         ))])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        // };
        // pub static ref MOVSDrm: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVSDrm)
        //         .set_uses(vec![TargetOperand::Mem])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        // };
        // pub static ref MOVSDmr: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVSDmr).set_uses(vec![
        //         TargetOperand::Mem,
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //     ])
        // };
        // pub static ref MOVSXDr64m32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVSXDr64m32)
        //         .set_uses(vec![TargetOperand::FrameIndex])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        // };
        // pub static ref LEAr64m: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::LEAr64m)
        //         .set_uses(vec![TargetOperand::Mem])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        // };
        // pub static ref ADDrr32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::ADDrr32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref ADDrr64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::ADDrr64)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref ADDri32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::ADDri32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //             TargetOperand::Immediate(TargetImmediate::I32),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref ADDr64i32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::ADDr64i32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
        //             TargetOperand::Immediate(TargetImmediate::I32),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref ADDSDrr: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::ADDSDrr)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref ADDSDrm: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::ADDSDrm)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //             TargetOperand::Mem,
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref SUBrr32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::SUBrr32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref SUBri32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::SUBri32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //             TargetOperand::Immediate(TargetImmediate::I32),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref SUBr64i32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::SUBr64i32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
        //             TargetOperand::Immediate(TargetImmediate::I32),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref SUBSDrr: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::SUBSDrr)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref SUBSDrm: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::SUBSDrm)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //             TargetOperand::Mem,
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref IMULrr32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::IMULrr32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref IMULrri32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::IMULrri32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //             TargetOperand::Immediate(TargetImmediate::I32),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        // };
        // pub static ref IMULrr64i32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::IMULrr64i32)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
        //             TargetOperand::Immediate(TargetImmediate::I32),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref MULSDrr: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MULSDrr)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref MULSDrm: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MULSDrm)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //             TargetOperand::Mem,
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref DIVSDrr: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::DIVSDrr)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref DIVSDrm: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::DIVSDrm)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
        //             TargetOperand::Mem,
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref SHLr32i8: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::SHLr32i8)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //             TargetOperand::Immediate(TargetImmediate::I8),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref SHLr64i8: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::SHLr64i8)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
        //             TargetOperand::Immediate(TargetImmediate::I8),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref SQRTSDrr: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::SQRTSDrr)
        //         .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
        //             RegisterClassKind::XMM,
        //         ))])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        //         // .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        // pub static ref CDQ: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::CDQ)
        //         .set_imp_def(vec![TargetRegister::Specific(GR32::EDX.as_phys_reg())])
        //         .set_imp_use(vec![TargetRegister::Specific(GR32::EAX.as_phys_reg())])
        // };
        // pub static ref MOVrr32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVrr32)
        //         .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
        //             RegisterClassKind::GR32,
        //         ))])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        // };
        // pub static ref MOVri32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVri32)
        //         .set_uses(vec![TargetOperand::Immediate(TargetImmediate::I32)])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        // };
        // pub static ref MOVrm32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVrm32)
        //         .set_uses(vec![TargetOperand::Mem])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        // };
        // pub static ref MOVmr32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVmr32).set_uses(vec![
        //         TargetOperand::Mem,
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
        //     ])
        // };
        // pub static ref MOVmi32: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVmi32).set_uses(vec![
        //         TargetOperand::Mem,
        //         TargetOperand::Immediate(TargetImmediate::I32),
        //     ])
        // };
        // pub static ref MOVmr64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVmr64).set_uses(vec![
        //         TargetOperand::Mem,
        //         TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
        //     ])
        // };
        // pub static ref MOVmi64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVmi64).set_uses(vec![
        //         TargetOperand::Mem,
        //         TargetOperand::Immediate(TargetImmediate::I64),
        //     ])
        // };
        // pub static ref MOVrr64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVrr64)
        //         .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
        //             RegisterClassKind::GR64,
        //         ))])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        // };
        // pub static ref MOVri64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVri64)
        //         .set_uses(vec![TargetOperand::Immediate(TargetImmediate::I64)])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        // };
        // pub static ref MOVrm64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::MOVrm64)
        //         .set_uses(vec![TargetOperand::Mem])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        // };
        // pub static ref IDIV: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::IDIV)
        //         .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
        //             RegisterClassKind::GR32,
        //         ))])
        //         .set_imp_def(vec![
        //             TargetRegister::Specific(GR32::EAX.as_phys_reg()),
        //             TargetRegister::Specific(GR32::EDX.as_phys_reg()),
        //         ])
        //         .set_imp_use(vec![
        //             TargetRegister::Specific(GR32::EAX.as_phys_reg()),
        //             TargetRegister::Specific(GR32::EDX.as_phys_reg()),
        //         ])
        // };
        // pub static ref PUSH64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::PUSH64).set_uses(vec![TargetOperand::Register(
        //         TargetRegister::RegClass(RegisterClassKind::GR64),
        //     )])
        // };
        // pub static ref POP64: TargetInstDef = {
        //     TargetInstDef::new(TargetOpcode::POP64).set_uses(vec![TargetOperand::Register(
        //         TargetRegister::RegClass(RegisterClassKind::GR64),
        //     )])
        // };
        // pub static ref RET: TargetInstDef = TargetInstDef::new(TargetOpcode::RET);

        pub static ref LI: TargetInstDef = TargetInstDef::new(TargetOpcode::LI).set_uses(
            vec![TargetOperand::Any]
        ).set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GPR)]);
        pub static ref JR: TargetInstDef = TargetInstDef::new(TargetOpcode::JR).set_uses(
            vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GPR))]
        );
    }
}

#[derive(Clone)]
pub struct TargetInstDef {
    pub opcode: TargetOpcode,
    pub uses: Vec<TargetOperand>,
    pub defs: Vec<TargetRegister>,
    pub tie: FxHashMap<DefOrUseReg, DefOrUseReg>, // def -> use
    pub imp_use: Vec<TargetRegister>,
    pub imp_def: Vec<TargetRegister>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum DefOrUseReg {
    Def(usize), // nth def register
    Use(usize), // nth use register
}

#[derive(Clone)]
pub enum TargetOperand {
    Register(TargetRegister),
    Immediate(TargetImmediate),
    Addr,
    FrameIndex,
    Any,
    Mem,
}

#[derive(Clone)]
pub enum TargetRegister {
    RegClass(RegisterClassKind),
    Specific(PhysReg),
}

#[derive(Clone, Copy, PartialEq)]
pub enum TargetImmediate {
    I8,
    I32,
    I64,
    F64,
}

// r => register
// i => constant integer
// m => TODO: [memory] or [rbp - fi.off]
// p => [register]
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TargetOpcode {
    // CALL,

    // Comparison
    Seteq,
    Setle,
    Setlt,

    LI, // Load Immediate
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
            Self::LI => Some(&*inst::LI),
            Self::JR => Some(&*inst::JR),
            // Self::MOVSDrm64 => Some(&*inst::MOVSDrm64),
            // Self::MOVSDmr => Some(&*inst::MOVSDmr),
            // Self::MOVSDrm => Some(&*inst::MOVSDrm),
            // Self::MOVSDrr => Some(&*inst::MOVSDrr),
            // Self::MOVSXDr64m32 => Some(&*inst::MOVSXDr64m32),
            // Self::LEAr64m => Some(&*inst::LEAr64m),
            // Self::ADDrr32 => Some(&*inst::ADDrr32),
            // Self::ADDrr64 => Some(&*inst::ADDrr64),
            // Self::ADDri32 => Some(&*inst::ADDri32),
            // Self::ADDr64i32 => Some(&*inst::ADDr64i32),
            // Self::ADDSDrr => Some(&*inst::ADDSDrr),
            // Self::ADDSDrm => Some(&*inst::ADDSDrm),
            // Self::SUBrr32 => Some(&*inst::SUBrr32),
            // Self::SUBri32 => Some(&*inst::SUBri32),
            // Self::SUBr64i32 => Some(&*inst::SUBr64i32),
            // Self::SUBSDrr => Some(&*inst::SUBSDrr),
            // Self::SUBSDrm => Some(&*inst::SUBSDrm),
            // Self::IMULrr32 => Some(&*inst::IMULrr32),
            // Self::IMULrri32 => Some(&*inst::IMULrri32),
            // Self::IMULrr64i32 => Some(&*inst::IMULrr64i32),
            // Self::MULSDrr => Some(&*inst::MULSDrr),
            // Self::MULSDrm => Some(&*inst::MULSDrm),
            // Self::CDQ => Some(&*inst::CDQ),
            // Self::DIVSDrr => Some(&*inst::DIVSDrr),
            // Self::DIVSDrm => Some(&*inst::DIVSDrm),
            // Self::SHLr64i8 => Some(&*inst::SHLr64i8),
            // Self::SHLr32i8 => Some(&*inst::SHLr32i8),
            // Self::SQRTSDrr => Some(&*inst::SQRTSDrr),
            // Self::MOVrr32 => Some(&*inst::MOVrr32),
            // Self::MOVri32 => Some(&*inst::MOVri32),
            // Self::MOVrm32 => Some(&*inst::MOVrm32),
            // Self::MOVmr32 => Some(&*inst::MOVmr32),
            // Self::MOVmi32 => Some(&*inst::MOVmi32),
            // Self::MOVmr64 => Some(&*inst::MOVmr64),
            // Self::MOVmi64 => Some(&*inst::MOVmi64),
            // Self::MOVrr64 => Some(&*inst::MOVrr64),
            // Self::MOVri64 => Some(&*inst::MOVri64),
            // Self::MOVrm64 => Some(&*inst::MOVrm64),
            // Self::IDIV => Some(&*inst::IDIV),
            // Self::PUSH64 => Some(&*inst::PUSH64),
            // Self::POP64 => Some(&*inst::POP64),
            // Self::RET => Some(&*inst::RET),
            _ => None,
        }
    }
}

impl TargetInstDef {
    pub fn new(opcode: TargetOpcode) -> Self {
        Self {
            opcode,
            uses: vec![],
            defs: vec![],
            tie: FxHashMap::default(),
            imp_def: vec![],
            imp_use: vec![],
        }
    }

    pub fn set_uses(mut self, uses: Vec<TargetOperand>) -> Self {
        self.uses = uses;
        self
    }

    pub fn set_defs(mut self, defs: Vec<TargetRegister>) -> Self {
        self.defs = defs;
        self
    }

    pub fn add_tie(mut self, def: DefOrUseReg, use_: DefOrUseReg) -> Self {
        self.tie.insert(def, use_);
        self
    }

    pub fn set_imp_def(mut self, imp_def: Vec<TargetRegister>) -> Self {
        self.imp_def = imp_def;
        self
    }

    pub fn set_imp_use(mut self, imp_use: Vec<TargetRegister>) -> Self {
        self.imp_use = imp_use;
        self
    }
}

impl TargetRegister {
    pub fn as_reg_class(&self) -> RegisterClassKind {
        match self {
            Self::RegClass(rc) => *rc,
            _ => panic!(),
        }
    }
}

impl DefOrUseReg {
    pub fn as_def(&self) -> usize {
        match self {
            Self::Def(d) => *d,
            _ => panic!(),
        }
    }

    pub fn as_use(&self) -> usize {
        match self {
            Self::Use(u) => *u,
            _ => panic!(),
        }
    }
}
