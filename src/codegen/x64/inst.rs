use super::{frame_object::FrameIndexInfo, register::*};
use std::fmt;

#[derive(Clone)]
pub struct TargetInstDef {
    pub opcode: TargetOpcode,
    pub uses: Vec<TargetOperand>,
    pub defs: Vec<TargetRegister>,
    // pub tie: FxHashMap<DefOrUseReg, DefOrUseReg>,
    // pub imp_use: Vec<MachineRegister>,
    // pub imp_def: Vec<MachineRegister>,
}

#[derive(Debug, Clone, Copy)]
pub enum DefOrUseReg {
    Def(usize), // nth def register
    Use(usize), // nth use register
}

#[derive(Clone)]
pub enum TargetOperand {
    Register(TargetRegister),
    Immediate(TargetImmediate),
    FrameIndex(FrameIndexInfo),
}

#[derive(Clone)]
pub enum TargetRegister {
    RegClass(RegisterClassKind),
    Specific(PhysReg),
}

#[derive(Clone, Copy, PartialEq)]
pub enum TargetImmediate {
    Int32(i32),
    Int64(i64),
    F64(f64),
}

// r => register
// i => constant integer
// m => TODO: [memory] or [rbp - fi.off]
// p => [register]
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TargetOpcode {
    MOVSDrm64,  // out(xmm) = movsd [memory]
    MOVrmi32,   // out = mov [rbp - fi.off + const.off]
    MOVrmri32,  // out = mov [rbp - fi.off + off * align]
    MOVrrri32,  // out = mov [base + off * align]
    MOVmi32r32, // mov [rbp - fi.off + const.off], reg
    MOVmi32i32, // mov [rbp - fi.off + const.off], const.val
    MOVpi32,    // mov [reg], const.val
    MOVpr32,    // mov [reg], reg
    MOVrp32,    // mov reg, [reg]
    StoreFiOff,
    StoreRegOff,

    MOVSXDr64m32, // out = movsxd [rbp - fi.off]

    LEArmi32, // out = lea [rbp - fi.off + const.off]
    LEArmr64, // out = lea [rbp - fi.off + reg]

    ADDrr32,
    ADDri32,
    ADDr64i32,
    SUBrr32,
    SUBri32,
    SUBr64i32,
    IMULrr32,
    IMULrri32,
    IMULrr64i32,
    CDQ,
    MOVrr32,
    MOVri32,
    MOVrm32,
    MOVmr32,
    MOVmi32,
    MOVrr64,
    MOVri64,
    MOVrm64,
    LEA64,
    IDIV,
    PUSH64,
    POP64,
    RET,

    Copy,

    // Call
    Call,

    // Binary arithmetics
    Add,

    // Comparison
    Seteq,
    Setle,
    Setlt,

    // Branch
    BrCond,
    Br,
    BrccEq,
    BrccLe,
    BrccLt,

    Phi,

    Ret,
}

impl TargetImmediate {
    pub fn as_i32(&self) -> i32 {
        match self {
            TargetImmediate::Int32(i) => *i,
            _ => panic!(),
        }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            TargetImmediate::Int64(i) => *i,
            _ => panic!(),
        }
    }

    pub fn as_f64(&self) -> f64 {
        match self {
            TargetImmediate::F64(f) => *f,
            _ => panic!(),
        }
    }
}
impl fmt::Debug for TargetImmediate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TargetImmediate::Int32(x) => write!(f, "i32 {}", x),
            TargetImmediate::Int64(x) => write!(f, "i64 {}", x),
            TargetImmediate::F64(x) => write!(f, "f64 {}", x),
        }
    }
}
