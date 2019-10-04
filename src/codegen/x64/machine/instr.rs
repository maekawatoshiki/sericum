use super::super::register::*;
use super::{basic_block::*, frame_object::*};
use crate::ir::types::*;
use bimap::BiMap;
use id_arena::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
    rc::Rc,
};

pub type RegisterInfoRef = Rc<RefCell<RegisterInfo>>;
pub type MachineInstrId = Id<MachineInstr>;

/// Bidirectional map for interconversion between MachineInstrId and an index for
/// ``MachineBasicBlock.iseq``
pub struct MachineInstrIdAndIndexBiMap {
    map: BiMap<MachineInstrId, usize>,
}

#[derive(Clone)]
pub struct MachineInstr {
    pub opcode: MachineOpcode,
    pub operand: Vec<MachineOperand>,
    pub ty: Type, // TODO: will be removed
    pub def: Vec<MachineRegister>,
    pub tie: FxHashMap<MachineRegister, MachineRegister>, // def -> use
    pub imp_use: Vec<MachineRegister>,
    pub imp_def: Vec<MachineRegister>,
    pub parent: MachineBasicBlockId,
}

#[derive(Clone, PartialEq)]
pub struct RegisterInfo {
    pub vreg: VirtReg,
    pub reg: Option<PhysReg>,
    pub ty: Type,
    pub spill: bool,
    pub last_use: Option<MachineInstrId>,
    pub use_list: FxHashSet<MachineInstrId>,
    pub def_list: FxHashSet<MachineInstrId>,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum MachineOpcode {
    // x86_64
    CDQ,
    MOV32rr,
    MOV32ri,
    IDIV,

    // Memory
    Load,
    Store,
    Copy,

    LoadFiConstOff,
    LoadFiOff,
    LoadRegOff,
    StoreFiConstOff,
    StoreFiOff,
    StoreRegOff,

    // Call
    Call,

    // Binary arithmetics
    Add,
    Sub,
    Mul,
    Rem,

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

    // Phi
    Phi,

    // Return
    Ret,
}

#[derive(Clone)]
pub enum MachineOperand {
    Register(MachineRegister),
    Constant(MachineConstant),
    FrameIndex(FrameIndexInfo),
    GlobalAddress(GlobalValueInfo),
    Branch(MachineBasicBlockId),
    None,
}

#[derive(Clone, Copy, PartialEq)]
pub enum MachineConstant {
    Int32(i32),
}

#[derive(Clone)]
pub enum GlobalValueInfo {
    FunctionName(String),
}

#[derive(Clone, PartialEq)]
pub struct MachineRegister {
    pub info: RegisterInfoRef,
}

impl ::std::cmp::Eq for MachineRegister {}
impl ::std::hash::Hash for MachineRegister {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.info.borrow().vreg.get());
        if let Some(reg) = self.info_ref().reg {
            state.write_usize(reg.get())
        }
    }
}

impl MachineInstrIdAndIndexBiMap {
    pub fn new() -> Self {
        Self { map: BiMap::new() }
    }

    pub fn insert(&mut self, id: MachineInstrId, idx: usize) {
        self.map.insert(id, idx);
    }

    pub fn get_index_by_instr_id(&self, id: MachineInstrId) -> Option<usize> {
        self.map.get_by_left(&id).map(|x| *x)
    }

    pub fn get_instr_id_by_index(&self, idx: usize) -> Option<MachineInstrId> {
        self.map.get_by_right(&idx).map(|x| *x)
    }
}

impl MachineInstr {
    pub fn new(
        vreg_gen: &VirtRegGen,
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        ty: Type,
        parent: MachineBasicBlockId,
    ) -> Self {
        Self {
            opcode,
            operand,
            def: match ty.clone() {
                Type::Void => vec![],
                ty => vec![vreg_gen.gen_vreg(ty).into_machine_register()],
            },
            ty,
            tie: FxHashMap::default(),
            imp_def: vec![],
            imp_use: vec![],
            parent,
        }
    }

    pub fn new_simple(
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        parent: MachineBasicBlockId,
    ) -> Self {
        Self {
            opcode,
            operand,
            def: vec![],
            ty: Type::Void,
            tie: FxHashMap::default(),
            imp_def: vec![],
            imp_use: vec![],
            parent,
        }
    }

    pub fn new_with_def_reg(
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        ty: Type,
        def: Vec<MachineRegister>,
        parent: MachineBasicBlockId,
    ) -> Self {
        Self {
            opcode,
            operand,
            def,
            ty,
            tie: FxHashMap::default(),
            imp_def: vec![],
            imp_use: vec![],
            parent,
        }
    }

    pub fn new_with_imp_def_use(
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        ty: Type,
        imp_def: Vec<MachineRegister>,
        imp_use: Vec<MachineRegister>,
        parent: MachineBasicBlockId,
    ) -> Self {
        Self {
            opcode,
            operand,
            def: vec![],
            ty,
            tie: FxHashMap::default(),
            imp_def,
            imp_use,
            parent,
        }
    }

    pub fn with_def(mut self, def: Vec<MachineRegister>) -> Self {
        self.def = def;
        self
    }

    pub fn with_type(mut self, ty: Type) -> Self {
        self.ty = ty;
        self
    }

    pub fn with_imp_use(mut self, r: MachineRegister) -> Self {
        self.imp_use.push(r);
        self
    }

    pub fn with_imp_uses(mut self, mut rs: Vec<MachineRegister>) -> Self {
        self.imp_use.append(&mut rs);
        self
    }

    pub fn add_use(&self, id: MachineInstrId) {
        for operand in &self.operand {
            let reg = match operand {
                MachineOperand::Register(reg) => reg,
                _ => continue,
            };
            reg.info_ref_mut().use_list.insert(id);
        }

        for reg in &self.imp_use {
            reg.info_ref_mut().use_list.insert(id);
        }
    }

    pub fn add_def(&self, id: MachineInstrId) {
        for reg in &self.def {
            reg.info_ref_mut().use_list.insert(id);
        }

        for reg in &self.imp_def {
            reg.info_ref_mut().use_list.insert(id);
        }
    }

    pub fn replace_operand_reg(&mut self, from: &MachineRegister, to: &MachineRegister) {
        for operand in self.operand.iter_mut() {
            match operand {
                MachineOperand::Register(ref mut r) if r.get_vreg() == from.get_vreg() => {
                    *r = to.clone();
                }
                _ => {}
            }
        }
    }

    pub fn set_tie(mut self, def: MachineRegister, use_: MachineRegister) -> Self {
        self.tie.insert(def, use_);
        self
    }

    pub fn set_tie_with_def(self, use_: MachineRegister) -> Self {
        let def = self.def[0].clone();
        self.set_tie(def, use_)
    }

    pub fn set_vreg(&self, vreg: VirtReg) {
        let x = &self.def[0];
        x.info_ref_mut().vreg = vreg;
    }

    pub fn set_last_use(&self, last_use: Option<MachineInstrId>) {
        let x = &self.def[0];
        x.info_ref_mut().last_use = last_use;
    }

    pub fn set_phy_reg(&self, reg: PhysReg, spill: bool) {
        let mut reg_info = self.def[0].info_ref_mut();
        reg_info.reg = Some(reg);
        reg_info.spill = spill;
    }

    pub fn get_last_use(&self) -> Option<MachineInstrId> {
        self.def[0].info_ref().last_use
    }

    pub fn get_vreg(&self) -> VirtReg {
        self.def[0].info_ref().vreg
    }

    pub fn get_reg(&self) -> Option<PhysReg> {
        self.def[0].info_ref().reg
    }
}

impl MachineOpcode {
    pub fn is_terminator(&self) -> bool {
        match self {
            MachineOpcode::Ret
            | MachineOpcode::Br
            | MachineOpcode::BrCond
            | MachineOpcode::BrccEq
            | MachineOpcode::BrccLe => true,
            _ => false,
        }
    }
}

impl MachineRegister {
    pub fn new(info: RegisterInfoRef) -> Self {
        Self { info }
    }

    pub fn copy_with_new_vreg(&self, vreg_gen: &VirtRegGen) -> Self {
        let r: RegisterInfo = self.info_ref().clone();
        r.copy_with_new_vreg(vreg_gen).into_machine_register()
    }

    pub fn set_vreg(&self, vreg: VirtReg) {
        self.info.borrow_mut().set_vreg(vreg);
    }

    pub fn set_last_use(&self, last_use: Option<MachineInstrId>) {
        self.info.borrow_mut().last_use = last_use;
    }

    pub fn set_phy_reg(&self, reg: PhysReg, spill: bool) {
        let mut reg_info = self.info.borrow_mut();
        reg_info.reg = Some(reg);
        reg_info.spill = spill;
    }

    pub fn get_last_use(&self) -> Option<MachineInstrId> {
        self.info.borrow().last_use
    }

    pub fn get_vreg(&self) -> VirtReg {
        self.info.borrow().vreg
    }

    pub fn get_reg(&self) -> Option<PhysReg> {
        self.info.borrow().reg
    }

    pub fn info_ref(&self) -> Ref<RegisterInfo> {
        self.info.borrow()
    }

    pub fn info_ref_mut(&self) -> RefMut<RegisterInfo> {
        self.info.borrow_mut()
    }

    pub fn is_vreg(&self) -> bool {
        self.info_ref().reg.is_none()
    }
}

impl RegisterInfo {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            vreg: VirtReg(0),
            reg: None,
            spill: false,
            last_use: None,
            use_list: FxHashSet::default(),
            def_list: FxHashSet::default(),
        }
    }

    pub fn new_phy_reg(ty: Type, reg: PhysReg) -> Self {
        Self {
            ty,
            vreg: VirtReg(0),
            reg: Some(reg),
            spill: false,
            last_use: None,
            use_list: FxHashSet::default(),
            def_list: FxHashSet::default(),
        }
    }

    pub fn new_ref(ty: Type) -> RegisterInfoRef {
        Rc::new(RefCell::new(Self {
            ty,
            vreg: VirtReg(0),
            reg: None,
            spill: false,
            last_use: None,
            use_list: FxHashSet::default(),
            def_list: FxHashSet::default(),
        }))
    }

    pub fn copy_with_new_vreg(&self, vreg_gen: &VirtRegGen) -> Self {
        let mut new = self.clone();
        new.vreg = vreg_gen.next_vreg();
        new
    }

    pub fn into_ref(self) -> RegisterInfoRef {
        Rc::new(RefCell::new(self))
    }

    pub fn into_machine_register(self) -> MachineRegister {
        MachineRegister::new(self.into_ref())
    }

    pub fn set_vreg(&mut self, vreg: VirtReg) {
        self.vreg = vreg;
    }

    pub fn with_vreg(mut self, vreg: VirtReg) -> Self {
        self.vreg = vreg;
        self
    }

    pub fn is_vreg(&self) -> bool {
        self.reg.is_none()
    }
}

impl MachineOperand {
    pub fn as_frame_index(&self) -> &FrameIndexInfo {
        match self {
            MachineOperand::FrameIndex(fi) => fi,
            _ => panic!(),
        }
    }

    pub fn as_register(&self) -> &MachineRegister {
        match self {
            MachineOperand::Register(r) => r,
            _ => panic!(),
        }
    }

    pub fn as_constant(&self) -> &MachineConstant {
        match self {
            MachineOperand::Constant(c) => c,
            _ => panic!(),
        }
    }

    pub fn as_basic_block(&self) -> MachineBasicBlockId {
        match self {
            MachineOperand::Branch(id) => *id,
            _ => panic!(),
        }
    }

    pub fn is_virtual_register(&self) -> bool {
        match self {
            MachineOperand::Register(r) => r.is_vreg(),
            _ => false,
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            MachineOperand::Branch(_) => None,
            MachineOperand::Constant(MachineConstant::Int32(_)) => Some(Type::Int32),
            MachineOperand::FrameIndex(fi) => Some(fi.ty.clone()),
            MachineOperand::GlobalAddress(_) => None, // TODO
            MachineOperand::None => None,
            MachineOperand::Register(r) => Some(r.info_ref().ty.clone()),
        }
    }
}

impl MachineConstant {
    pub fn as_i32(&self) -> i32 {
        match self {
            MachineConstant::Int32(i) => *i,
        }
    }
}

impl fmt::Debug for MachineInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, def) in self.def.iter().enumerate() {
            def.fmt(f)?;
            if i < self.def.len() - 1 {
                write!(f, ", ")?;
            } else {
                write!(f, " = ")?;
            }
        }

        write!(f, "{:?} ", self.opcode)?;

        for (i, op) in self.operand.iter().enumerate() {
            op.fmt(f)?;
            if i < self.operand.len() - 1 {
                write!(f, ", ")?;
            }
        }

        if self.tie.len() > 0 || self.imp_def.len() > 0 || self.imp_use.len() > 0 {
            write!(f, " (")?;
        }

        if self.tie.len() != 0 {
            write!(f, "tie:")?;
            for (def, use_) in &self.tie {
                write!(f, "{:?}->{:?},", def, use_)?;
            }
        }

        if self.imp_def.len() != 0 {
            write!(f, "imp-def:")?;
            for reg in &self.imp_def {
                write!(f, "{:?},", reg)?;
            }
        }

        if self.imp_use.len() != 0 {
            write!(f, "imp-use:")?;
            for reg in &self.imp_use {
                write!(f, "{:?},", reg)?;
            }
        }

        if self.tie.len() > 0 || self.imp_def.len() > 0 || self.imp_use.len() > 0 {
            write!(f, ")")?;
        }

        fmt::Result::Ok(())
    }
}

impl fmt::Debug for MachineOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineOperand::Register(r) => r.fmt(f),
            MachineOperand::Constant(c) => c.fmt(f),
            MachineOperand::FrameIndex(fi) => fi.fmt(f),
            MachineOperand::GlobalAddress(g) => g.fmt(f),
            MachineOperand::Branch(id) => write!(f, "BB#{}", id.index()),
            MachineOperand::None => write!(f, ""),
        }
    }
}

impl fmt::Debug for MachineRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.info_ref().fmt(f)
    }
}

impl fmt::Debug for RegisterInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.reg {
            Some(phy_reg) => phy_reg.fmt(f),
            None => self.vreg.fmt(f),
        }
    }
}

impl fmt::Debug for MachineConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineConstant::Int32(i) => write!(f, "i32 {}", i),
        }
    }
}

impl fmt::Debug for GlobalValueInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GlobalValueInfo::FunctionName(name) => write!(f, "ga<{}>", name),
        }
    }
}
