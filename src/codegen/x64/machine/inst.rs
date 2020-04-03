pub use super::super::inst::{TargetImmediate, TargetOpcode};
use super::super::register::{
    rc2ty, PhysReg, RegisterClassKind, TargetRegisterTrait, VirtReg, VirtRegGen,
};
use super::{basic_block::*, const_data::DataId, frame_object::*};
use crate::ir::types::*;
use id_arena::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
    rc::Rc,
};

pub type MachineOpcode = TargetOpcode;
pub type RegisterInfoRef = Rc<RefCell<RegisterInfo>>;
pub type MachineInstId = Id<MachineInst>;

#[derive(Clone)]
pub struct MachineInst {
    pub opcode: MachineOpcode,
    pub operand: Vec<MachineOperand>,
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
    pub reg_class: RegisterClassKind,
    pub tied: Option<RegisterInfoRef>,
    pub use_list: FxHashSet<MachineInstId>,
    pub def_list: FxHashSet<MachineInstId>,
}

#[derive(Clone)]
pub struct MachineInstDef {
    pub opcode: MachineOpcode,
    pub operand: Vec<MachineOperand>, // reg|imm|
    pub def: Vec<MachineRegister>,
    pub tie: FxHashMap<MachineRegister, MachineRegister>, // def -> use
    pub imp_use: Vec<MachineRegister>,
    pub imp_def: Vec<MachineRegister>,
    pub parent: MachineBasicBlockId,
}

#[derive(Clone)]
pub enum MachineOperand {
    Register(MachineRegister),
    Constant(MachineConstant),
    FrameIndex(FrameIndexInfo),
    Address(AddressInfo),
    Branch(MachineBasicBlockId),
    None,
}

#[derive(Clone)]
pub enum AddressInfo {
    FunctionName(String),
    Absolute(DataId),
}

#[derive(Clone, Copy, PartialEq)]
pub enum MachineConstant {
    Int32(i32),
    Int64(i64),
    F64(f64),
}

#[derive(Clone, PartialEq)]
pub struct MachineRegister {
    pub info: RegisterInfoRef,
}

impl ::std::cmp::Eq for MachineRegister {}
impl ::std::hash::Hash for MachineRegister {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.info.borrow().vreg.retrieve());
        if let Some(reg) = self.info_ref().reg {
            state.write_usize(reg.retrieve())
        }
    }
}

impl MachineInst {
    pub fn new(
        vreg_gen: &VirtRegGen,
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        rc: Option<RegisterClassKind>,
        // ty: &Type,
        parent: MachineBasicBlockId,
    ) -> Self {
        Self {
            opcode,
            operand,
            def: match rc {
                None => vec![],
                Some(rc) => vec![vreg_gen.gen_vreg(rc).into_machine_register()],
            },
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
            tie: FxHashMap::default(),
            imp_def: vec![],
            imp_use: vec![],
            parent,
        }
    }

    pub fn new_with_def_reg(
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        def: Vec<MachineRegister>,
        parent: MachineBasicBlockId,
    ) -> Self {
        Self {
            opcode,
            operand,
            def,
            tie: FxHashMap::default(),
            imp_def: vec![],
            imp_use: vec![],
            parent,
        }
    }

    pub fn new_with_imp_def_use(
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        imp_def: Vec<MachineRegister>,
        imp_use: Vec<MachineRegister>,
        parent: MachineBasicBlockId,
    ) -> Self {
        Self {
            opcode,
            operand,
            def: vec![],
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

    pub fn with_imp_use(mut self, r: MachineRegister) -> Self {
        self.imp_use.push(r);
        self
    }

    pub fn with_imp_def(mut self, r: MachineRegister) -> Self {
        self.imp_def.push(r);
        self
    }

    pub fn with_imp_uses(mut self, mut rs: Vec<MachineRegister>) -> Self {
        self.imp_use.append(&mut rs);
        self
    }

    pub fn with_imp_defs(mut self, mut rs: Vec<MachineRegister>) -> Self {
        self.imp_def.append(&mut rs);
        self
    }

    pub fn add_use(&self, id: MachineInstId) {
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

    pub fn add_def(&self, id: MachineInstId) {
        for reg in &self.def {
            reg.info_ref_mut().def_list.insert(id);
        }

        for reg in &self.imp_def {
            reg.info_ref_mut().def_list.insert(id);
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

    pub fn tie_regs(&mut self, def: MachineRegister, use_: MachineRegister) {
        self.tie.insert(def, use_);
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

    pub fn set_phy_reg(&self, reg: PhysReg) {
        let mut reg_info = self.def[0].info_ref_mut();
        reg_info.reg = Some(reg);
    }

    pub fn get_vreg(&self) -> VirtReg {
        self.def[0].info_ref().vreg
    }

    pub fn get_reg(&self) -> Option<PhysReg> {
        self.def[0].info_ref().reg
    }

    pub fn has_def_reg(&self) -> bool {
        self.def.len() > 0
    }

    pub fn get_def_reg(&self) -> Option<&MachineRegister> {
        if !self.has_def_reg() {
            return None;
        }
        Some(&self.def[0])
    }

    pub fn collect_defined_regs(&self) -> Vec<MachineRegister> {
        let mut regs = self.def.clone();
        regs.append(&mut self.imp_def.clone());
        regs
    }

    pub fn collect_used_regs(&self) -> Vec<MachineRegister> {
        let mut regs = vec![];
        for operand in &self.operand {
            match operand {
                MachineOperand::Register(r) => {
                    if r.get_reg().is_none() {
                        regs.push(r.clone()) // TODO
                    }
                }
                _ => {}
            }
        }
        regs
    }
}

impl MachineOpcode {
    pub fn is_copy(&self) -> bool {
        matches!(
            self,
            MachineOpcode::MOVrr32 | MachineOpcode::MOVrr64 | MachineOpcode::Copy
        )
    }

    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            MachineOpcode::Ret
                | MachineOpcode::RET
                | MachineOpcode::JMP
                | MachineOpcode::BrCond
                | MachineOpcode::JE
                | MachineOpcode::JL
                | MachineOpcode::JLE
        )
    }
}

impl MachineRegister {
    pub fn new(info: RegisterInfoRef) -> Self {
        Self { info }
    }

    pub fn add_use(&self, use_id: MachineInstId) {
        self.info_ref_mut().use_list.insert(use_id);
    }

    pub fn add_def(&self, def_id: MachineInstId) {
        self.info_ref_mut().def_list.insert(def_id);
    }

    pub fn copy_with_new_vreg(&self, vreg_gen: &VirtRegGen) -> Self {
        let r: RegisterInfo = self.info_ref().clone();
        r.copy_with_new_vreg(vreg_gen).into_machine_register()
    }

    pub fn has_tied(&self) -> bool {
        self.info_ref().tied.is_some()
    }

    pub fn tie_reg(&self, r: &MachineRegister) {
        self.info_ref_mut().tie_reg(r.info.clone())
    }

    pub fn set_vreg(&self, vreg: VirtReg) {
        self.info.borrow_mut().set_vreg(vreg);
    }

    pub fn set_phy_reg(&self, reg: PhysReg) {
        let mut info = self.info_ref_mut();
        info.reg = Some(reg);

        if let Some(tied) = &info.tied {
            tied.borrow_mut().reg = Some(reg);
        }
    }

    pub fn get_vreg(&self) -> VirtReg {
        self.info.borrow().vreg
    }

    pub fn get_reg(&self) -> Option<PhysReg> {
        self.info.borrow().reg
    }

    pub fn get_reg_class(&self) -> RegisterClassKind {
        self.info.borrow().reg_class
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

    pub fn is_phys_reg(&self) -> bool {
        self.info_ref().reg.is_some()
    }
}

impl RegisterInfo {
    pub fn new(reg_class: RegisterClassKind) -> Self {
        Self {
            reg_class,
            vreg: VirtReg(0),
            reg: None,
            tied: None,
            use_list: FxHashSet::default(),
            def_list: FxHashSet::default(),
        }
    }

    pub fn new_phy_reg<T: TargetRegisterTrait>(reg: T) -> Self {
        Self {
            reg_class: reg.as_phys_reg().reg_class(),
            vreg: VirtReg(0),
            reg: Some(reg.as_phys_reg()),
            tied: None,
            use_list: FxHashSet::default(),
            def_list: FxHashSet::default(),
        }
    }

    pub fn new_ref(reg_class: RegisterClassKind) -> RegisterInfoRef {
        Rc::new(RefCell::new(Self {
            reg_class,
            vreg: VirtReg(0),
            reg: None,
            tied: None,
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

    pub fn tie_reg(&mut self, tied: RegisterInfoRef) {
        self.tied = Some(tied);
    }

    pub fn set_vreg(&mut self, vreg: VirtReg) {
        self.vreg = vreg;
    }

    pub fn with_vreg(mut self, vreg: VirtReg) -> Self {
        self.vreg = vreg;
        self
    }

    pub fn with_reg(mut self, reg: PhysReg) -> Self {
        self.reg = Some(reg);
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

    pub fn as_address(&self) -> &AddressInfo {
        match self {
            MachineOperand::Address(addr) => addr,
            _ => panic!(),
        }
    }

    pub fn is_virtual_register(&self) -> bool {
        match self {
            MachineOperand::Register(r) => r.is_vreg(),
            _ => false,
        }
    }

    pub fn is_register(&self) -> bool {
        matches!(self, MachineOperand::Register(_))
    }

    pub fn is_frame_index(&self) -> bool {
        matches!(self, MachineOperand::FrameIndex(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, MachineOperand::None)
    }

    pub fn is_const_i32(&self) -> bool {
        match self {
            MachineOperand::Constant(constant) => matches!(constant, MachineConstant::Int32(_)),
            _ => false,
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, MachineOperand::Constant(_))
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            MachineOperand::Branch(_) => None,
            MachineOperand::Constant(MachineConstant::Int32(_)) => Some(Type::Int32),
            MachineOperand::Constant(MachineConstant::Int64(_)) => Some(Type::Int64),
            MachineOperand::Constant(MachineConstant::F64(_)) => Some(Type::F64),
            MachineOperand::FrameIndex(fi) => Some(fi.ty.clone()),
            MachineOperand::Address(_) => None, // TODO
            MachineOperand::None => None,
            // TODO
            MachineOperand::Register(r) => Some(rc2ty(r.info_ref().reg_class)),
        }
    }
}

impl AddressInfo {
    pub fn as_absolute(&self) -> DataId {
        match self {
            AddressInfo::Absolute(id) => *id,
            _ => panic!(),
        }
    }
}

impl MachineConstant {
    pub fn size_in_byte(&self) -> usize {
        match self {
            MachineConstant::Int32(_) => 4,
            MachineConstant::Int64(_) => 8,
            MachineConstant::F64(_) => 8,
        }
    }

    pub fn size_in_bits(&self) -> usize {
        self.size_in_byte() * 8
    }
}

impl MachineConstant {
    pub fn as_i32(&self) -> i32 {
        match self {
            Self::Int32(i) => *i,
            _ => panic!(),
        }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            Self::Int64(i) => *i,
            _ => panic!(),
        }
    }

    pub fn as_f64(&self) -> f64 {
        match self {
            Self::F64(f) => *f,
            _ => panic!(),
        }
    }
}

impl fmt::Debug for MachineConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int32(x) => write!(f, "i32 {}", x),
            Self::Int64(x) => write!(f, "i64 {}", x),
            Self::F64(x) => write!(f, "f64 {}", x),
        }
    }
}

impl fmt::Debug for MachineInst {
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
            MachineOperand::Address(g) => g.fmt(f),
            MachineOperand::Branch(id) => write!(f, "BB#{}", id.index()),
            MachineOperand::None => write!(f, "!"),
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

impl fmt::Debug for AddressInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AddressInfo::FunctionName(name) => write!(f, "addr<fn:{}>", name),
            AddressInfo::Absolute(id) => write!(f, "addr<{}>", id),
        }
    }
}
