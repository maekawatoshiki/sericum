pub use crate::codegen::common::machine::register::*;
use crate::ir::types::Type;
use defs::registers;
use id_arena::Arena;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

registers! {
    class GPR (64, i64, [i32, i64, Pointer!], [A0, A1]) {
        ZERO, RA, SP, GP, TP, T0, T1, T2, S0, S1, A0,
        A1, A2, A3, A4, A5, A6, A7, S2, S3, S4, S5,
        S6, S7, S8, S9, S10, S11, T3, T4, T5, T6
    }

    order arg GPR { A0, A1, A2, A3, A4, A5, A6, A7 }

    order gp GPR { T0, T1, T2, A0, A1, A2, A3, A4,
                    A5, A6, A7, T3, T4, T5, T6, S2,
                    S3, S4, S5, S6, S7, S8, S9, S10, S11 } // S1 is reserved
}

macro_rules! to_phys {
    ($($r:path),*) => {
        vec![$(($r.as_phys_reg())),*]
    };
}

thread_local! {
    pub static CALLEE_SAVED_REGS: PhysRegSet = {
        let mut bits = PhysRegSet::new();
        let regs = to_phys![
            GPR::SP,
            GPR::S0,
            GPR::S1,
            GPR::S2,
            GPR::S3,
            GPR::S4,
            GPR::S5,
            GPR::S6,
            GPR::S7,
            GPR::S8,
            GPR::S9,
            GPR::S10,
            GPR::S11
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
