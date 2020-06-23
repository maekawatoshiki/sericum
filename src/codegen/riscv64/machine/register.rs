pub use crate::codegen::common::machine::register::*;
use crate::ir::types::Type;
use id_arena::Arena;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

const GPR_NUM: isize = 32;
pub const PHYS_REGISTERS_NUM: usize = (GPR_NUM) as usize;

// Remember to fix PhysReg::reg_class() when appending a variant
#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum RegisterClassKind {
    GPR = 0,
}

// TODO: TEMPORARY FUNCTIONS. WILL BE REMOVED.
pub fn ty2rc(ty: &Type) -> Option<RegisterClassKind> {
    match ty {
        Type::Void => None,
        Type::Int32 => Some(RegisterClassKind::GPR),
        Type::Int64 => Some(RegisterClassKind::GPR),
        Type::F64 => None,
        Type::Pointer(_) => Some(RegisterClassKind::GPR),
        Type::Array(_) => None,
        e => unimplemented!("{:?}", e),
    }
}

pub fn rc2ty(rc: RegisterClassKind) -> Type {
    match rc {
        RegisterClassKind::GPR => Type::Int64,
    }
}

impl PhysReg {
    pub fn reg_class(&self) -> RegisterClassKind {
        RegisterClassKind::GPR
    }
}

macro_rules! to_phys {
    ($($r:path),*) => {
        vec![$(($r.as_phys_reg())),*]
    };
}

impl RegisterClassKind {
    pub fn size_in_bits(&self) -> usize {
        match self {
            Self::GPR => 64,
        }
    }

    pub fn register_file_base_class(&self) -> RegisterClassKind {
        match self {
            Self::GPR => RegisterClassKind::GPR,
        }
    }

    // Returns normal order of registers used to pass arguments
    pub fn get_arg_reg_order_vec(&self) -> Vec<PhysReg> {
        match self {
            RegisterClassKind::GPR => to_phys!(
                GPR::A0,
                GPR::A1,
                GPR::A2,
                GPR::A3,
                GPR::A4,
                GPR::A5,
                GPR::A6,
                GPR::A7
            ),
        }
    }

    // Returns normal order of general-purpose registers
    pub fn get_gp_reg_order_vec(&self) -> Vec<PhysReg> {
        match self {
            RegisterClassKind::GPR => to_phys!(
                GPR::T0,
                GPR::T1,
                GPR::T2,
                GPR::A0,
                GPR::A1,
                GPR::A2,
                GPR::A3,
                GPR::A4,
                GPR::A5,
                GPR::A6,
                GPR::A7,
                GPR::T3,
                GPR::T4,
                GPR::T5,
                GPR::T6
            ),
        }
    }

    pub fn return_value_register(&self) -> PhysReg {
        match self {
            Self::GPR => GPR::A0.as_phys_reg(),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum GPR {
    ZERO,
    RA,
    SP,
    GP,
    TP,
    T0,
    T1,
    T2,
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl TargetRegisterTrait for PhysReg {
    fn as_phys_reg(&self) -> PhysReg {
        *self
    }

    fn sub_reg(&self) -> Option<PhysReg> {
        let r = self.as_phys_reg().retrieve();
        let subs: [Option<PhysReg>; PHYS_REGISTERS_NUM] = [
            GPR::ZERO.sub_reg(),
            GPR::RA.sub_reg(),
            GPR::SP.sub_reg(),
            GPR::GP.sub_reg(),
            GPR::TP.sub_reg(),
            GPR::T0.sub_reg(),
            GPR::T1.sub_reg(),
            GPR::T2.sub_reg(),
            GPR::S0.sub_reg(),
            GPR::S1.sub_reg(),
            GPR::A0.sub_reg(),
            GPR::A1.sub_reg(),
            GPR::A2.sub_reg(),
            GPR::A3.sub_reg(),
            GPR::A4.sub_reg(),
            GPR::A5.sub_reg(),
            GPR::A6.sub_reg(),
            GPR::A7.sub_reg(),
            GPR::S2.sub_reg(),
            GPR::S3.sub_reg(),
            GPR::S4.sub_reg(),
            GPR::S5.sub_reg(),
            GPR::S6.sub_reg(),
            GPR::S7.sub_reg(),
            GPR::S8.sub_reg(),
            GPR::S9.sub_reg(),
            GPR::S10.sub_reg(),
            GPR::S11.sub_reg(),
            GPR::T3.sub_reg(),
            GPR::T4.sub_reg(),
            GPR::T5.sub_reg(),
            GPR::T6.sub_reg(),
        ];
        subs[r]
    }

    fn super_reg(&self) -> Option<PhysReg> {
        let r = self.as_phys_reg().retrieve();
        let supers: [Option<PhysReg>; PHYS_REGISTERS_NUM] = [
            GPR::ZERO.super_reg(),
            GPR::RA.super_reg(),
            GPR::SP.super_reg(),
            GPR::GP.super_reg(),
            GPR::TP.super_reg(),
            GPR::T0.super_reg(),
            GPR::T1.super_reg(),
            GPR::T2.super_reg(),
            GPR::S0.super_reg(),
            GPR::S1.super_reg(),
            GPR::A0.super_reg(),
            GPR::A1.super_reg(),
            GPR::A2.super_reg(),
            GPR::A3.super_reg(),
            GPR::A4.super_reg(),
            GPR::A5.super_reg(),
            GPR::A6.super_reg(),
            GPR::A7.super_reg(),
            GPR::S2.super_reg(),
            GPR::S3.super_reg(),
            GPR::S4.super_reg(),
            GPR::S5.super_reg(),
            GPR::S6.super_reg(),
            GPR::S7.super_reg(),
            GPR::S8.super_reg(),
            GPR::S9.super_reg(),
            GPR::S10.super_reg(),
            GPR::S11.super_reg(),
            GPR::T3.super_reg(),
            GPR::T4.super_reg(),
            GPR::T5.super_reg(),
            GPR::T6.super_reg(),
        ];
        supers[r]
    }

    fn regs_sharing_same_register_file(&self) -> PhysRegSet {
        if let Some(set) = REG_FILE.with(|f| f.borrow().get(self).map(|s| s.clone())) {
            return set;
        }
        let mut set = PhysRegSet::new();
        let mut cur = *self;
        set.set(*self);
        while let Some(r) = cur.sub_reg() {
            set.set(r);
            cur = r;
        }
        while let Some(r) = cur.super_reg() {
            set.set(r);
            cur = r;
        }
        REG_FILE.with(|f| f.borrow_mut().insert(*self, set.clone()));
        set
    }
}

impl TargetRegisterTrait for GPR {
    fn as_phys_reg(&self) -> PhysReg {
        PhysReg(*self as usize + RegisterClassKind::GPR as usize)
    }

    fn sub_reg(&self) -> Option<PhysReg> {
        None
    }

    fn super_reg(&self) -> Option<PhysReg> {
        None
    }

    fn regs_sharing_same_register_file(&self) -> PhysRegSet {
        self.as_phys_reg().regs_sharing_same_register_file()
    }
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

impl PhysReg {
    pub fn name(&self) -> &str {
        let reg_names = [
            "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3",
            "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
            "t3", "t4", "t5", "t6",
        ];
        reg_names[self.retrieve()]
    }
}

impl RegistersInfo {
    pub fn new() -> Self {
        let mut arena = Arena::new();
        let mut phys_regs_list = vec![];

        fn f<T: TargetRegisterTrait>(arena: &mut Arena<RegisterInfo>, r: T) -> RegisterId {
            let id = arena.alloc(RegisterInfo::new_phys_reg(r));
            RegisterId {
                id,
                kind: VirtOrPhys::Phys(r.as_phys_reg()),
            }
        }
        phys_regs_list.push(f(&mut arena, GPR::ZERO));
        phys_regs_list.push(f(&mut arena, GPR::RA));
        phys_regs_list.push(f(&mut arena, GPR::SP));
        phys_regs_list.push(f(&mut arena, GPR::GP));
        phys_regs_list.push(f(&mut arena, GPR::TP));
        phys_regs_list.push(f(&mut arena, GPR::T0));
        phys_regs_list.push(f(&mut arena, GPR::T1));
        phys_regs_list.push(f(&mut arena, GPR::T2));
        phys_regs_list.push(f(&mut arena, GPR::S0));
        phys_regs_list.push(f(&mut arena, GPR::S1));
        phys_regs_list.push(f(&mut arena, GPR::A0));
        phys_regs_list.push(f(&mut arena, GPR::A1));
        phys_regs_list.push(f(&mut arena, GPR::A2));
        phys_regs_list.push(f(&mut arena, GPR::A3));
        phys_regs_list.push(f(&mut arena, GPR::A4));
        phys_regs_list.push(f(&mut arena, GPR::A5));
        phys_regs_list.push(f(&mut arena, GPR::A6));
        phys_regs_list.push(f(&mut arena, GPR::A7));
        phys_regs_list.push(f(&mut arena, GPR::S2));
        phys_regs_list.push(f(&mut arena, GPR::S3));
        phys_regs_list.push(f(&mut arena, GPR::S4));
        phys_regs_list.push(f(&mut arena, GPR::S5));
        phys_regs_list.push(f(&mut arena, GPR::S6));
        phys_regs_list.push(f(&mut arena, GPR::S7));
        phys_regs_list.push(f(&mut arena, GPR::S8));
        phys_regs_list.push(f(&mut arena, GPR::S9));
        phys_regs_list.push(f(&mut arena, GPR::S10));
        phys_regs_list.push(f(&mut arena, GPR::S11));
        phys_regs_list.push(f(&mut arena, GPR::T3));
        phys_regs_list.push(f(&mut arena, GPR::T4));
        phys_regs_list.push(f(&mut arena, GPR::T5));
        phys_regs_list.push(f(&mut arena, GPR::T6));

        Self {
            arena: RefCell::new(RegisterArena(arena)),
            cur_virt_reg: RefCell::new(0),
            phys_regs_list,
        }
    }
}

pub fn str2reg(s: &str) -> Option<PhysReg> {
    Some(match s.to_ascii_lowercase().as_str() {
        "zero" => GPR::ZERO.as_phys_reg(),
        "ra" => GPR::RA.as_phys_reg(),
        "sp" => GPR::SP.as_phys_reg(),
        "gp" => GPR::GP.as_phys_reg(),
        "tp" => GPR::TP.as_phys_reg(),
        "t0" => GPR::T0.as_phys_reg(),
        "t1" => GPR::T1.as_phys_reg(),
        "t2" => GPR::T2.as_phys_reg(),
        "s0" => GPR::S0.as_phys_reg(),
        "s1" => GPR::S1.as_phys_reg(),
        "a0" => GPR::A0.as_phys_reg(),
        "a1" => GPR::A1.as_phys_reg(),
        "a2" => GPR::A2.as_phys_reg(),
        "a3" => GPR::A3.as_phys_reg(),
        "a4" => GPR::A4.as_phys_reg(),
        "a5" => GPR::A5.as_phys_reg(),
        "a6" => GPR::A6.as_phys_reg(),
        "a7" => GPR::A7.as_phys_reg(),
        "s2" => GPR::S2.as_phys_reg(),
        "s3" => GPR::S3.as_phys_reg(),
        "s4" => GPR::S4.as_phys_reg(),
        "s5" => GPR::S5.as_phys_reg(),
        "s6" => GPR::S6.as_phys_reg(),
        "s7" => GPR::S7.as_phys_reg(),
        "s8" => GPR::S8.as_phys_reg(),
        "s9" => GPR::S9.as_phys_reg(),
        "s10" => GPR::S10.as_phys_reg(),
        "s11" => GPR::S11.as_phys_reg(),
        "t3" => GPR::T3.as_phys_reg(),
        "t4" => GPR::T4.as_phys_reg(),
        "t5" => GPR::T5.as_phys_reg(),
        "t6" => GPR::T6.as_phys_reg(),
        _ => return None,
    })
}
