pub use crate::codegen::common::machine::register::*;
use crate::ir::types::Type;
use id_arena::Arena;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

macro_rules! to_phys {
    ($($r:path),*) => {
        vec![$(($r.as_phys_reg())),*]
    };
}

// TODO: TEMPORARY FUNCTIONS. WILL BE REMOVED.
pub fn ty2rc(ty: &Type) -> Option<RegisterClassKind> {
    match ty {
        Type::Void => None,
        Type::Int32 => Some(RegisterClassKind::GR32),
        Type::Int64 => Some(RegisterClassKind::GR64),
        Type::F64 => Some(RegisterClassKind::XMM),
        Type::Pointer(_) => Some(RegisterClassKind::GR64),
        Type::Array(_) => None,
        e => unimplemented!("{:?}", e),
    }
}

pub fn rc2ty(rc: RegisterClassKind) -> Type {
    match rc {
        RegisterClassKind::GR32 => Type::Int32,
        RegisterClassKind::GR64 => Type::Int64,
        RegisterClassKind::XMM => Type::F64,
    }
}

// Remember to fix PhysReg::reg_class() when appending a variant
#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum RegisterClassKind {
    GR32 = 0,
    GR64 = GR32_NUM,
    XMM = GR32_NUM + GR64_NUM,
}

impl PhysReg {
    pub fn reg_class(&self) -> RegisterClassKind {
        // TODO
        let n = self.retrieve();
        if RegisterClassKind::GR32 as usize <= n && n < RegisterClassKind::GR64 as usize {
            return RegisterClassKind::GR32;
        } else if RegisterClassKind::GR64 as usize <= n && n < RegisterClassKind::XMM as usize {
            return RegisterClassKind::GR64;
        }
        RegisterClassKind::XMM
    }
}

impl RegisterClassKind {
    pub fn get_reg_order(&self) -> RegisterOrder {
        RegisterOrder::general_purpose(*self)
    }

    pub fn get_arg_reg_order(&self) -> RegisterOrder {
        RegisterOrder::arguments(*self)
    }

    pub fn get_nth_arg_reg(&self, nth: usize) -> Option<PhysReg> {
        self.get_arg_reg_order_vec().get(nth).map(|r| *r)
    }

    pub fn size_in_bits(&self) -> usize {
        match self {
            Self::GR32 => 32,
            Self::GR64 => 64,
            Self::XMM => 128,
        }
    }

    pub fn size_in_byte(&self) -> usize {
        self.size_in_bits() / 8
    }

    pub fn shares_same_register_file(&self, rc: RegisterClassKind) -> bool {
        self.register_file_base_class() == rc.register_file_base_class()
    }

    pub fn register_file_base_class(&self) -> RegisterClassKind {
        match self {
            Self::GR32 | Self::GR64 => RegisterClassKind::GR32,
            Self::XMM => RegisterClassKind::XMM,
        }
    }

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
                // GR32::ESI,
                // GR32::EDI,
                GR32::R8D,
                GR32::R9D,
                GR32::R10D,
                GR32::R11D
                // GR32::R12D,
                // GR32::R13D,
                // GR32::R14D,
                // GR32::R15D,
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
                // GR64::R12,
                // GR64::R13,
                // GR64::R14,
                // GR64::R15,
            ),
            RegisterClassKind::XMM => to_phys!(
                XMM::XMM0,
                XMM::XMM1,
                XMM::XMM2,
                XMM::XMM3,
                XMM::XMM4,
                XMM::XMM5 // XMM::XMM6,
                          // XMM::XMM7,
                          // XMM::XMM8,
                          // XMM::XMM9,
                          // XMM::XMM10,
                          // XMM::XMM11,
                          // XMM::XMM12,
                          // XMM::XMM13,
                          // XMM::XMM14,
                          // XMM::XMM15
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

// TODO: The definition of GR32 is now hard coded in ROOT/defs/src/register.rs
use defs::registers;
registers! {
    class GR32 (32, Int32) < GR64 {
        EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
        R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D
    }

    class GR64 (64, Int64) {
        RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
        R8, R9, R10, R11, R12, R13, R14, R15
    }

    class XMM (128, F64) {
        XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
        XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15
    }
}

impl TargetRegisterTrait for PhysReg {
    fn as_phys_reg(&self) -> PhysReg {
        *self
    }

    fn sub_reg(&self) -> Option<PhysReg> {
        let r = self.as_phys_reg().retrieve();
        let subs: [Option<PhysReg>; PHYS_REGISTERS_NUM] = [
            GR32::EAX.sub_reg(),
            GR32::ECX.sub_reg(),
            GR32::EDX.sub_reg(),
            GR32::EBX.sub_reg(),
            GR32::ESP.sub_reg(),
            GR32::EBP.sub_reg(),
            GR32::ESI.sub_reg(),
            GR32::EDI.sub_reg(),
            GR32::R8D.sub_reg(),
            GR32::R9D.sub_reg(),
            GR32::R10D.sub_reg(),
            GR32::R11D.sub_reg(),
            GR32::R12D.sub_reg(),
            GR32::R13D.sub_reg(),
            GR32::R14D.sub_reg(),
            GR32::R15D.sub_reg(),
            GR64::RAX.sub_reg(),
            GR64::RCX.sub_reg(),
            GR64::RDX.sub_reg(),
            GR64::RBX.sub_reg(),
            GR64::RSP.sub_reg(),
            GR64::RBP.sub_reg(),
            GR64::RSI.sub_reg(),
            GR64::RDI.sub_reg(),
            GR64::R8.sub_reg(),
            GR64::R9.sub_reg(),
            GR64::R10.sub_reg(),
            GR64::R11.sub_reg(),
            GR64::R12.sub_reg(),
            GR64::R13.sub_reg(),
            GR64::R14.sub_reg(),
            GR64::R15.sub_reg(),
            XMM::XMM0.sub_reg(),
            XMM::XMM1.sub_reg(),
            XMM::XMM2.sub_reg(),
            XMM::XMM3.sub_reg(),
            XMM::XMM4.sub_reg(),
            XMM::XMM5.sub_reg(),
            XMM::XMM6.sub_reg(),
            XMM::XMM7.sub_reg(),
            XMM::XMM8.sub_reg(),
            XMM::XMM9.sub_reg(),
            XMM::XMM10.sub_reg(),
            XMM::XMM11.sub_reg(),
            XMM::XMM12.sub_reg(),
            XMM::XMM13.sub_reg(),
            XMM::XMM14.sub_reg(),
            XMM::XMM15.sub_reg(),
        ];
        subs[r]
    }

    fn super_reg(&self) -> Option<PhysReg> {
        let r = self.as_phys_reg().retrieve();
        let supers: [Option<PhysReg>; PHYS_REGISTERS_NUM] = [
            GR32::EAX.super_reg(),
            GR32::ECX.super_reg(),
            GR32::EDX.super_reg(),
            GR32::EBX.super_reg(),
            GR32::ESP.super_reg(),
            GR32::EBP.super_reg(),
            GR32::ESI.super_reg(),
            GR32::EDI.super_reg(),
            GR32::R8D.super_reg(),
            GR32::R9D.super_reg(),
            GR32::R10D.super_reg(),
            GR32::R11D.super_reg(),
            GR32::R12D.super_reg(),
            GR32::R13D.super_reg(),
            GR32::R14D.super_reg(),
            GR32::R15D.super_reg(),
            GR64::RAX.super_reg(),
            GR64::RCX.super_reg(),
            GR64::RDX.super_reg(),
            GR64::RBX.super_reg(),
            GR64::RSP.super_reg(),
            GR64::RBP.super_reg(),
            GR64::RSI.super_reg(),
            GR64::RDI.super_reg(),
            GR64::R8.super_reg(),
            GR64::R9.super_reg(),
            GR64::R10.super_reg(),
            GR64::R11.super_reg(),
            GR64::R12.super_reg(),
            GR64::R13.super_reg(),
            GR64::R14.super_reg(),
            GR64::R15.super_reg(),
            XMM::XMM0.super_reg(),
            XMM::XMM1.super_reg(),
            XMM::XMM2.super_reg(),
            XMM::XMM3.super_reg(),
            XMM::XMM4.super_reg(),
            XMM::XMM5.super_reg(),
            XMM::XMM6.super_reg(),
            XMM::XMM7.super_reg(),
            XMM::XMM8.super_reg(),
            XMM::XMM9.super_reg(),
            XMM::XMM10.super_reg(),
            XMM::XMM11.super_reg(),
            XMM::XMM12.super_reg(),
            XMM::XMM13.super_reg(),
            XMM::XMM14.super_reg(),
            XMM::XMM15.super_reg(),
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

// register nubmering: https://corsix.github.io/dynasm-doc/instructions.html#registers

impl TargetRegisterTrait for GR32 {
    fn as_phys_reg(&self) -> PhysReg {
        PhysReg(*self as usize + RegisterClassKind::GR32 as usize)
    }

    fn sub_reg(&self) -> Option<PhysReg> {
        None
    }

    fn super_reg(&self) -> Option<PhysReg> {
        Some(match self {
            GR32::EAX => GR64::RAX.as_phys_reg(),
            GR32::ECX => GR64::RCX.as_phys_reg(),
            GR32::EDX => GR64::RDX.as_phys_reg(),
            GR32::EBX => GR64::RBX.as_phys_reg(),
            GR32::ESP => GR64::RSP.as_phys_reg(),
            GR32::EBP => GR64::RBP.as_phys_reg(),
            GR32::ESI => GR64::RSI.as_phys_reg(),
            GR32::EDI => GR64::RDI.as_phys_reg(),
            GR32::R8D => GR64::R8.as_phys_reg(),
            GR32::R9D => GR64::R9.as_phys_reg(),
            GR32::R10D => GR64::R10.as_phys_reg(),
            GR32::R11D => GR64::R11.as_phys_reg(),
            GR32::R12D => GR64::R12.as_phys_reg(),
            GR32::R13D => GR64::R13.as_phys_reg(),
            GR32::R14D => GR64::R14.as_phys_reg(),
            GR32::R15D => GR64::R15.as_phys_reg(),
        })
    }

    fn regs_sharing_same_register_file(&self) -> PhysRegSet {
        self.as_phys_reg().regs_sharing_same_register_file()
    }
}

impl TargetRegisterTrait for GR64 {
    fn as_phys_reg(&self) -> PhysReg {
        PhysReg(*self as usize + RegisterClassKind::GR64 as usize)
    }

    fn sub_reg(&self) -> Option<PhysReg> {
        Some(match self {
            GR64::RAX => GR32::EAX.as_phys_reg(),
            GR64::RCX => GR32::ECX.as_phys_reg(),
            GR64::RDX => GR32::EDX.as_phys_reg(),
            GR64::RBX => GR32::EBX.as_phys_reg(),
            GR64::RSP => GR32::ESP.as_phys_reg(),
            GR64::RBP => GR32::EBP.as_phys_reg(),
            GR64::RSI => GR32::ESI.as_phys_reg(),
            GR64::RDI => GR32::EDI.as_phys_reg(),
            GR64::R8 => GR32::R8D.as_phys_reg(),
            GR64::R9 => GR32::R9D.as_phys_reg(),
            GR64::R10 => GR32::R10D.as_phys_reg(),
            GR64::R11 => GR32::R11D.as_phys_reg(),
            GR64::R12 => GR32::R12D.as_phys_reg(),
            GR64::R13 => GR32::R13D.as_phys_reg(),
            GR64::R14 => GR32::R14D.as_phys_reg(),
            GR64::R15 => GR32::R15D.as_phys_reg(),
        })
    }

    fn super_reg(&self) -> Option<PhysReg> {
        None
    }

    fn regs_sharing_same_register_file(&self) -> PhysRegSet {
        self.as_phys_reg().regs_sharing_same_register_file()
    }
}

impl TargetRegisterTrait for XMM {
    fn as_phys_reg(&self) -> PhysReg {
        PhysReg(*self as usize + RegisterClassKind::XMM as usize)
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

impl PhysReg {
    pub fn name(&self) -> &str {
        let reg_names = [
            "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d",
            "r12d", "r13d", "r14d", "r15d", "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
            "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", "xmm0", "xmm1", "xmm2", "xmm3",
            "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13",
            "xmm14", "xmm15",
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

        phys_regs_list.push(f(&mut arena, GR32::EAX));
        phys_regs_list.push(f(&mut arena, GR32::ECX));
        phys_regs_list.push(f(&mut arena, GR32::EDX));
        phys_regs_list.push(f(&mut arena, GR32::EBX));
        phys_regs_list.push(f(&mut arena, GR32::ESP));
        phys_regs_list.push(f(&mut arena, GR32::EBP));
        phys_regs_list.push(f(&mut arena, GR32::ESI));
        phys_regs_list.push(f(&mut arena, GR32::EDI));
        phys_regs_list.push(f(&mut arena, GR32::R8D));
        phys_regs_list.push(f(&mut arena, GR32::R9D));
        phys_regs_list.push(f(&mut arena, GR32::R10D));
        phys_regs_list.push(f(&mut arena, GR32::R11D));
        phys_regs_list.push(f(&mut arena, GR32::R12D));
        phys_regs_list.push(f(&mut arena, GR32::R13D));
        phys_regs_list.push(f(&mut arena, GR32::R14D));
        phys_regs_list.push(f(&mut arena, GR32::R15D));
        phys_regs_list.push(f(&mut arena, GR64::RAX));
        phys_regs_list.push(f(&mut arena, GR64::RCX));
        phys_regs_list.push(f(&mut arena, GR64::RDX));
        phys_regs_list.push(f(&mut arena, GR64::RBX));
        phys_regs_list.push(f(&mut arena, GR64::RSP));
        phys_regs_list.push(f(&mut arena, GR64::RBP));
        phys_regs_list.push(f(&mut arena, GR64::RSI));
        phys_regs_list.push(f(&mut arena, GR64::RDI));
        phys_regs_list.push(f(&mut arena, GR64::R8));
        phys_regs_list.push(f(&mut arena, GR64::R9));
        phys_regs_list.push(f(&mut arena, GR64::R10));
        phys_regs_list.push(f(&mut arena, GR64::R11));
        phys_regs_list.push(f(&mut arena, GR64::R12));
        phys_regs_list.push(f(&mut arena, GR64::R13));
        phys_regs_list.push(f(&mut arena, GR64::R14));
        phys_regs_list.push(f(&mut arena, GR64::R15));
        phys_regs_list.push(f(&mut arena, XMM::XMM0));
        phys_regs_list.push(f(&mut arena, XMM::XMM1));
        phys_regs_list.push(f(&mut arena, XMM::XMM2));
        phys_regs_list.push(f(&mut arena, XMM::XMM3));
        phys_regs_list.push(f(&mut arena, XMM::XMM4));
        phys_regs_list.push(f(&mut arena, XMM::XMM5));
        phys_regs_list.push(f(&mut arena, XMM::XMM6));
        phys_regs_list.push(f(&mut arena, XMM::XMM7));
        phys_regs_list.push(f(&mut arena, XMM::XMM8));
        phys_regs_list.push(f(&mut arena, XMM::XMM9));
        phys_regs_list.push(f(&mut arena, XMM::XMM10));
        phys_regs_list.push(f(&mut arena, XMM::XMM11));
        phys_regs_list.push(f(&mut arena, XMM::XMM12));
        phys_regs_list.push(f(&mut arena, XMM::XMM13));
        phys_regs_list.push(f(&mut arena, XMM::XMM14));
        phys_regs_list.push(f(&mut arena, XMM::XMM15));

        Self {
            arena: RefCell::new(RegisterArena(arena)),
            cur_virt_reg: RefCell::new(0),
            phys_regs_list,
        }
    }
}

pub fn str2reg(s: &str) -> Option<PhysReg> {
    Some(match s.to_ascii_lowercase().as_str() {
        "eax" => GR32::EAX.as_phys_reg(),
        "ecx" => GR32::ECX.as_phys_reg(),
        "edx" => GR32::EDX.as_phys_reg(),
        "ebx" => GR32::EBX.as_phys_reg(),
        "esp" => GR32::ESP.as_phys_reg(),
        "ebp" => GR32::EBP.as_phys_reg(),
        "esi" => GR32::ESI.as_phys_reg(),
        "edi" => GR32::EDI.as_phys_reg(),
        "r8d" => GR32::R8D.as_phys_reg(),
        "r9d" => GR32::R9D.as_phys_reg(),
        "r10d" => GR32::R10D.as_phys_reg(),
        "r11d" => GR32::R11D.as_phys_reg(),
        "r12d" => GR32::R12D.as_phys_reg(),
        "r13d" => GR32::R13D.as_phys_reg(),
        "r14d" => GR32::R14D.as_phys_reg(),
        "r15d" => GR32::R15D.as_phys_reg(),
        "rax" => GR64::RAX.as_phys_reg(),
        "rcx" => GR64::RCX.as_phys_reg(),
        "rdx" => GR64::RDX.as_phys_reg(),
        "rbx" => GR64::RBX.as_phys_reg(),
        "rsp" => GR64::RSP.as_phys_reg(),
        "rbp" => GR64::RBP.as_phys_reg(),
        "rsi" => GR64::RSI.as_phys_reg(),
        "rdi" => GR64::RDI.as_phys_reg(),
        "r8" => GR64::R8.as_phys_reg(),
        "r9" => GR64::R9.as_phys_reg(),
        "r10" => GR64::R10.as_phys_reg(),
        "r11" => GR64::R11.as_phys_reg(),
        "r12" => GR64::R12.as_phys_reg(),
        "r13" => GR64::R13.as_phys_reg(),
        "r14" => GR64::R14.as_phys_reg(),
        "r15" => GR64::R15.as_phys_reg(),
        "xmm0" => XMM::XMM0.as_phys_reg(),
        "xmm1" => XMM::XMM1.as_phys_reg(),
        "xmm2" => XMM::XMM2.as_phys_reg(),
        "xmm3" => XMM::XMM3.as_phys_reg(),
        "xmm4" => XMM::XMM4.as_phys_reg(),
        "xmm5" => XMM::XMM5.as_phys_reg(),
        "xmm6" => XMM::XMM6.as_phys_reg(),
        "xmm7" => XMM::XMM7.as_phys_reg(),
        "xmm8" => XMM::XMM8.as_phys_reg(),
        "xmm9" => XMM::XMM9.as_phys_reg(),
        "xmm10" => XMM::XMM10.as_phys_reg(),
        "xmm11" => XMM::XMM11.as_phys_reg(),
        "xmm12" => XMM::XMM12.as_phys_reg(),
        "xmm13" => XMM::XMM13.as_phys_reg(),
        "xmm14" => XMM::XMM14.as_phys_reg(),
        "xmm15" => XMM::XMM15.as_phys_reg(),
        _ => return None,
    })
}
