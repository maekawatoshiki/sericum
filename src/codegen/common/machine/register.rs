use crate::codegen::arch::machine::inst::MachineInstId;
use crate::codegen::arch::register::*;
use id_arena::{Arena, Id};
use rustc_hash::FxHashSet;
use std::fmt::Debug;
use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
    hash::Hash,
    ops::{BitAnd, BitOr},
    ops::{Index, IndexMut},
};

pub trait TargetRegisterTrait: Copy + Clone {
    fn as_phys_reg(&self) -> PhysReg;
    fn sub_reg(&self) -> Option<PhysReg>;
    fn super_reg(&self) -> Option<PhysReg>;
    fn regs_sharing_same_register_file(&self) -> PhysRegSet;
}

#[derive(Debug, Clone)]
pub struct PhysRegSet(pub [u64; 1]); // 8*8*1 > PHYS_REGISTERS_NUM

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct VirtReg(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct PhysReg(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct RegisterId {
    pub id: Id<RegisterInfo>,
    pub kind: VirtOrPhys,
}

#[derive(Clone, Debug)]
pub struct RegisterInfo {
    pub virt_reg: VirtReg,
    pub phys_reg: Option<PhysReg>,
    pub reg_class: RegisterClassKind,
    pub tied: Option<RegisterId>,
    pub uses: FxHashSet<MachineInstId>,
    pub defs: FxHashSet<MachineInstId>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum VirtOrPhys {
    Virt(VirtReg),
    Phys(PhysReg),
}

#[derive(Clone)]
pub struct RegistersInfo {
    pub cur_virt_reg: RefCell<usize>,
    pub arena: RefCell<RegisterArena>,
    pub phys_regs_list: Vec<RegisterId>,
}

#[derive(Clone)]
pub struct RegisterArena(pub Arena<RegisterInfo>);

#[derive(Debug, Clone)]
pub struct RegisterOrder {
    order: Vec<PhysReg>,
    nth: usize,
    reg_class: RegisterClassKind,
}

impl VirtReg {
    pub fn retrieve(&self) -> usize {
        self.0
    }
}

impl PhysReg {
    pub fn retrieve(&self) -> usize {
        self.0
    }
}

impl RegistersInfo {
    pub fn new_virt_reg(&self, reg_class: RegisterClassKind) -> RegisterId {
        let virt_reg = self.gen_virt_reg();
        let id = self.arena.borrow_mut().0.alloc(RegisterInfo {
            virt_reg,
            phys_reg: None,
            reg_class,
            tied: None,
            uses: FxHashSet::default(),
            defs: FxHashSet::default(),
        });
        RegisterId {
            id,
            kind: VirtOrPhys::Virt(virt_reg),
        }
    }

    pub fn get_phys_reg<T: TargetRegisterTrait>(&self, r: T) -> RegisterId {
        let i = r.as_phys_reg().retrieve();
        self.phys_regs_list[i]
    }

    pub fn arena_ref_mut(&self) -> RefMut<RegisterArena> {
        self.arena.borrow_mut()
    }

    pub fn arena_ref(&self) -> Ref<RegisterArena> {
        self.arena.borrow()
    }

    fn gen_virt_reg(&self) -> VirtReg {
        let n = *self.cur_virt_reg.borrow();
        *self.cur_virt_reg.borrow_mut() += 1;
        VirtReg(n)
    }
}

impl RegisterInfo {
    pub fn new_phys_reg<T: TargetRegisterTrait>(r: T) -> Self {
        RegisterInfo {
            virt_reg: VirtReg(0),
            phys_reg: Some(r.as_phys_reg()),
            reg_class: r.as_phys_reg().reg_class(),
            tied: None,
            uses: FxHashSet::default(),
            defs: FxHashSet::default(),
        }
    }

    pub fn add_def(&mut self, id: MachineInstId) {
        self.defs.insert(id);
    }

    pub fn remove_def(&mut self, id: MachineInstId) {
        self.defs.remove(&id);
    }

    pub fn add_use(&mut self, id: MachineInstId) {
        self.uses.insert(id);
    }

    pub fn remove_use(&mut self, id: MachineInstId) {
        self.uses.remove(&id);
    }
}

impl RegisterId {
    pub fn is_virt_reg(&self) -> bool {
        matches!(self.kind, VirtOrPhys::Virt(_))
    }

    pub fn is_phys_reg(&self) -> bool {
        matches!(self.kind, VirtOrPhys::Phys(_))
    }

    pub fn as_phys_reg(&self) -> PhysReg {
        match self.kind {
            VirtOrPhys::Phys(p) => p,
            VirtOrPhys::Virt(_) => panic!(),
        }
    }

    pub fn as_virt_reg(&self) -> VirtReg {
        match self.kind {
            VirtOrPhys::Virt(v) => v,
            VirtOrPhys::Phys(_) => panic!(),
        }
    }
}

impl Index<RegisterId> for RegisterArena {
    type Output = RegisterInfo;

    fn index(&self, id: RegisterId) -> &Self::Output {
        &self.0[id.id]
    }
}

impl IndexMut<RegisterId> for RegisterArena {
    fn index_mut(&mut self, id: RegisterId) -> &mut Self::Output {
        &mut self.0[id.id]
    }
}

impl PhysRegSet {
    pub fn new() -> Self {
        Self([0; 1])
    }

    pub fn set<T: TargetRegisterTrait>(&mut self, r: T) {
        self.0[0] |= 1 << r.as_phys_reg().retrieve();
    }

    pub fn has<T: TargetRegisterTrait>(&self, r: T) -> bool {
        (self.0[0] & (1 << r.as_phys_reg().retrieve())) != 0
    }

    pub fn to_phys_set(&self) -> FxHashSet<PhysReg> {
        let mut set = FxHashSet::default();
        for i in 0..self.0.len() * 8 * 8 {
            if (self.0[0] & (1 << i)) != 0 {
                set.insert(PhysReg(i));
            }
        }
        set
    }

    pub fn unite(&mut self, rhs: &Self) {
        self.0[0] |= rhs.0[0];
    }
}

impl BitOr for PhysRegSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self([self.0[0] & rhs.0[0]])
    }
}

impl BitAnd for PhysRegSet {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self([self.0[0] & rhs.0[0]])
    }
}

impl RegisterOrder {
    pub fn general_purpose(reg_class: RegisterClassKind) -> Self {
        Self {
            reg_class,
            order: reg_class.get_gp_reg_order_vec(),
            nth: 0,
        }
    }

    pub fn arguments(reg_class: RegisterClassKind) -> Self {
        Self {
            reg_class,
            order: reg_class.get_arg_reg_order_vec(),
            nth: 0,
        }
    }

    pub fn add_preferred_reg(&mut self, r: PhysReg) {
        self.order.insert(0, r);
    }
}

impl Iterator for RegisterOrder {
    type Item = PhysReg;

    fn next(&mut self) -> Option<Self::Item> {
        self.nth += 1;
        self.order.get(self.nth - 1).and_then(|item| Some(*item))
    }
}

impl fmt::Debug for PhysReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.name())
    }
}

impl fmt::Debug for VirtReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%vreg{}", self.retrieve())
    }
}

impl fmt::Debug for VirtOrPhys {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Phys(p) => p.fmt(f),
            Self::Virt(v) => v.fmt(f),
        }
    }
}

impl fmt::Debug for RegisterId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            VirtOrPhys::Phys(p) => p.fmt(f)?,
            VirtOrPhys::Virt(v) => v.fmt(f)?,
        }
        write!(f, ":{}", self.id.index())
    }
}
