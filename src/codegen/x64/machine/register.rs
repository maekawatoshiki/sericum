pub use crate::codegen::common::machine::register::*;
use crate::ir::types::Type;
use defs::registers;
use id_arena::Arena;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

registers! {
    class GR32 (32, Int32, [Int32]) < GR64 {
        EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
        R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D
    }

    class GR64 (64, Int64, [Int64, Pointer!]) {
        RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
        R8, R9, R10, R11, R12, R13, R14, R15
    }

    class XMM (128, F64, [F64]) {
        XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
        XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15
    }

    // order gp GR32 {
    //     EAX
    // }
}

macro_rules! to_phys {
    ($($r:path),*) => {
        vec![$(($r.as_phys_reg())),*]
    };
}

impl RegisterClassKind {
    // Returns normal order of registers used to pass arguments
    // TODO: This is System V AMD64 ABI.
    // https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI
    pub fn get_arg_reg_order_vec(&self) -> Vec<PhysReg> {
        match self {
            RegisterClassKind::GR32 => to_phys!(
                GR32::EDI,
                GR32::ESI,
                GR32::EDX,
                GR32::ECX,
                GR32::R8D,
                GR32::R9D
            ),
            RegisterClassKind::GR64 => to_phys!(
                GR64::RDI,
                GR64::RSI,
                GR64::RDX,
                GR64::RCX,
                GR64::R8,
                GR64::R9
            ),
            RegisterClassKind::XMM => to_phys!(
                XMM::XMM0,
                XMM::XMM1,
                XMM::XMM2,
                XMM::XMM3,
                XMM::XMM4,
                XMM::XMM5,
                XMM::XMM6,
                XMM::XMM7
            ),
        }
    }

    // Returns normal order of general-purpose registers
    pub fn get_gp_reg_order_vec(&self) -> Vec<PhysReg> {
        // commented-out registers are callee-saved registers
        match self {
            RegisterClassKind::GR32 => to_phys!(
                GR32::EAX,
                GR32::ECX,
                GR32::EDX,
                GR32::R8D,
                GR32::R9D,
                GR32::R10D,
                GR32::R11D
            ),
            RegisterClassKind::GR64 => to_phys!(
                GR64::RAX,
                GR64::RCX,
                GR64::RDX,
                // GR64::RSI,
                // GR64::RDI,
                GR64::R8,
                GR64::R9,
                GR64::R10,
                GR64::R11
            ),
            RegisterClassKind::XMM => to_phys!(
                XMM::XMM0,
                XMM::XMM1,
                XMM::XMM2,
                XMM::XMM3,
                XMM::XMM4,
                XMM::XMM5
            ),
        }
    }

    pub fn return_value_register(&self) -> PhysReg {
        match self {
            Self::GR32 => GR32::EAX.as_phys_reg(),
            Self::GR64 => GR64::RAX.as_phys_reg(),
            Self::XMM => XMM::XMM0.as_phys_reg(),
        }
    }
}

// register nubmering: https://corsix.github.io/dynasm-doc/instructions.html#registers

thread_local! {
    pub static CALLEE_SAVED_REGS: PhysRegSet = {
        let mut bits = PhysRegSet::new();
        let regs = to_phys![
            // GR32::EAX,
            // GR64::RAX,
            GR32::EBX,
            // GR32::ESP,
            GR32::EBP,
            GR32::R12D,
            GR32::R13D,
            GR32::R14D,
            GR32::R15D,
            GR64::RBX,
            // GR64::RSP,
            GR64::RBP,
            GR64::R12,
            GR64::R13,
            GR64::R14,
            GR64::R15,
            XMM::XMM6,
            XMM::XMM7,
            XMM::XMM8,
            XMM::XMM15
        ];
        for reg in regs {
            bits.set(reg)
        }
        bits
    };

    pub static REG_FILE: RefCell<FxHashMap<PhysReg, PhysRegSet>> = {
        RefCell::new(FxHashMap::default())
    }
}
