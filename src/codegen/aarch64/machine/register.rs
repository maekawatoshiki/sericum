pub use crate::codegen::common::machine::register::*;
use crate::ir::types::Type;
use defs::registers;
use id_arena::Arena;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

registers! {
    class SP (64, i64, [], [SP]) {
        SP
    }

    class WSP (32, i32, [], [WSP]) < SP {
        WSP
    }

    class GR64 (64, i64, [i64, Pointer!], [X0]) {
        X0, X1, X2, X3, X4,
        X5, X6, X7, X8, X9,
        X10, X11, X12, X13, X14,
        X15, X16, X17, X18, X19,
        X20, X21, X22, X23, X24,
        X25, X26, X27, X28, X29, X30
    }

    class GR32 (32, i32, [i32], [W0]) < GR64 {
        W0, W1, W2, W3, W4,
        W5, W6, W7, W8, W9,
        W10, W11, W12, W13, W14,
        W15, W16, W17, W18, W19,
        W20, W21, W22, W23, W24,
        W25, W26, W27, W28, W29, W30
    }

    order arg GR32 { W0, W1, W2, W3, W4, W5, W6, W7 }
    order arg GR64 { X0, X1, X2, X3, X4, X5, X6, X7 }
    order arg SP { SP } order arg WSP { WSP }

    order gp GR32 {
        W0, W1, W2, W3, W4, W5, W6, W7,
        W9, W10, W11, W12, W13, W14, W15,
        W19, W20, W21, W22, W23, W24, W25, W26, W27, W28
    }
    order gp GR64 {
        X0, X1, X2, X3, X4, X5, X6, X7,
        X9, X10, X11, X12, X13, X14, X15,
        X19, X20, X21, X22, X23, X24, X25, X26, X27, X28
    }
    order gp SP { SP } order gp WSP { WSP }
}

thread_local! {
    pub static CALLEE_SAVED_REGS: PhysRegSet = {
        let mut bits = PhysRegSet::new();
        let regs = to_phys![
            GR32::W19,
            GR32::W20,
            GR32::W21,
            GR32::W22,
            GR32::W23,
            GR32::W24,
            GR32::W25,
            GR32::W26,
            GR32::W27,
            GR32::W28,
            GR32::W29,
            GR32::W30,
            GR64::X19,
            GR64::X20,
            GR64::X21,
            GR64::X22,
            GR64::X23,
            GR64::X24,
            GR64::X25,
            GR64::X26,
            GR64::X27,
            GR64::X28,
            GR64::X29,
            GR64::X30
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
