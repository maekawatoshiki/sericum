pub use crate::codegen::common::machine::register::*;
use crate::ir::types::Type;
use defs::registers;
use id_arena::Arena;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

registers! {
    // register nubmering: https://corsix.github.io/dynasm-doc/instructions.html#registers
    class GR8  (8, i8, [i1, i8], [AL]) < GR32 {
        AL, CL, DL, BL, SPL, BPL, SIL, DIL,
        R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B
    }

    class GR32 (32, i32, [i32], [EAX]) < GR64 {
        EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
        R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D
    }

    class GR64 (64, i64, [i64, Pointer!], [RAX]) {
        RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
        R8, R9, R10, R11, R12, R13, R14, R15
    }

    class XMM (128, f64, [f64], [XMM0]) {
        XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
        XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15
    }

    // Normal order of registers used to pass arguments
    // TODO: This is System V AMD64 ABI.
    // https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI
    order arg GR8  { DIL, SIL, DL, CL, R8B, R9B }
    order arg GR32 { EDI, ESI, EDX, ECX, R8D, R9D }
    order arg GR64 { RDI, RSI, RDX, RCX, R8,  R9 }
    order arg XMM  { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 }

    // Normal order of general-purpose registers
    order gp GR8  { AL,  CL,  DL,  R8B, R9B, R10B, R11B, BL, R12B, R13B, R14B, R15B }
    order gp GR32 { EAX, ECX, EDX, R8D, R9D, R10D, R11D, EBX,R12D, R13D, R14D, R15D }
    order gp GR64 { RAX, RCX, RDX, R8,  R9,  R10,  R11,  RBX,R12,  R13,  R14,  R15  }
    order gp XMM { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM15 }
}

thread_local! {
    pub static CALLEE_SAVED_REGS: PhysRegSet = {
        let mut bits = PhysRegSet::new();
        let regs = to_phys![
            GR8::BL,
            GR8::BPL,
            GR8::R12B,
            GR8::R13B,
            GR8::R14B,
            GR8::R15B,
            GR32::EBX,
            GR32::EBP,
            GR32::R12D,
            GR32::R13D,
            GR32::R14D,
            GR32::R15D,
            GR64::RBX,
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
