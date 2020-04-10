pub use super::super::inst::{TargetImmediate, TargetOpcode};
use super::super::register::{
    rc2ty, PhysReg, RegisterClassKind, TargetRegisterTrait, VirtReg, VirtRegGen, GR32, GR64, XMM,
};
use super::{basic_block::*, const_data::DataId, frame_object::*};
use crate::ir::types::*;
use id_arena::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
    fmt::Debug,
    rc::Rc,
};

pub type MachineOpcode = TargetOpcode;
pub type RegisterInfoRef = Rc<RefCell<RegisterInfo>>;
pub type MachineInstId = Id<MachineInst>;

#[derive(Clone)]
pub struct MachineInst {
    pub id: Option<MachineInstId>,
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

#[derive(Debug, Clone)]
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
            id: None,
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
            id: None,
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
            id: None,
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
            id: None,
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

    pub fn set_def(&mut self, r: MachineRegister) {
        self.def[0].remove_def(self.id.unwrap());
        self.def[0] = r;
        self.def[0].add_def(self.id.unwrap());
    }

    pub fn set_id(&mut self, id: MachineInstId) {
        let old_id = self.id;
        self.id = Some(id);
        self.set_use_to_regs(old_id);
        self.set_def_to_regs(old_id);
    }

    pub fn set_use_to_regs(&self, old_id: Option<MachineInstId>) {
        let id = self.id.unwrap();

        for reg in self.operand.iter().filter_map(|o| match o {
            MachineOperand::Register(r) => Some(r),
            _ => None,
        }) {
            some_then!(id, old_id, reg.remove_use(id));
            reg.add_use(id);
        }

        for reg in &self.imp_use {
            some_then!(id, old_id, reg.remove_use(id));
            reg.add_use(id);
        }
    }

    pub fn set_def_to_regs(&self, old_id: Option<MachineInstId>) {
        let id = self.id.unwrap();

        for reg in &self.def {
            some_then!(id, old_id, reg.remove_def(id));
            reg.add_def(id);
        }

        for reg in &self.imp_def {
            some_then!(id, old_id, reg.remove_def(id));
            reg.add_def(id);
        }
    }

    pub fn replace_operand_register(&mut self, from: &MachineRegister, to: &MachineRegister) {
        // TODO: This loop may run once at most
        for r in self.operand.iter_mut().filter_map(|o| match o {
            MachineOperand::Register(r) if r.get_vreg() == from.get_vreg() => Some(r),
            _ => None,
        }) {
            r.remove_use(self.id.unwrap());
            *r = to.clone();
            r.add_use(self.id.unwrap());
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
    pub fn is_copy_like(&self) -> bool {
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

    pub fn phys_reg<T: TargetRegisterTrait>(r: T) -> Self {
        RegisterInfo::phys_reg(r).into_machine_register()
    }

    pub fn add_use(&self, use_id: MachineInstId) {
        self.info_ref_mut().use_list.insert(use_id);
    }

    pub fn remove_use(&self, id: MachineInstId) {
        self.info_ref_mut().use_list.remove(&id);
    }

    pub fn add_def(&self, def_id: MachineInstId) {
        self.info_ref_mut().def_list.insert(def_id);
    }

    pub fn remove_def(&self, id: MachineInstId) {
        self.info_ref_mut().def_list.remove(&id);
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

    pub fn phys_reg<T: TargetRegisterTrait>(reg: T) -> Self {
        PHYS_REGS.with(|r| r[reg.as_phys_reg().retrieve()].clone())
    }

    // not recommended
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
    pub fn phys_reg<T: TargetRegisterTrait>(r: T) -> MachineOperand {
        MachineOperand::Register(RegisterInfo::phys_reg(r).into_machine_register())
    }

    pub fn imm_i32(i: i32) -> Self {
        MachineOperand::Constant(MachineConstant::Int32(i))
    }

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

    pub fn is_address(&self) -> bool {
        matches!(self, MachineOperand::Address(_))
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

thread_local! {
    pub static PHYS_REGS: [RegisterInfo; 48] = {
        [
            RegisterInfo::new_phy_reg(GR32::EAX ),
            RegisterInfo::new_phy_reg(GR32::ECX ),
            RegisterInfo::new_phy_reg(GR32::EDX ),
            RegisterInfo::new_phy_reg(GR32::EBX ),
            RegisterInfo::new_phy_reg(GR32::ESP ),
            RegisterInfo::new_phy_reg(GR32::EBP ),
            RegisterInfo::new_phy_reg(GR32::ESI ),
            RegisterInfo::new_phy_reg(GR32::EDI ),
            RegisterInfo::new_phy_reg(GR32::R8D ),
            RegisterInfo::new_phy_reg(GR32::R9D ),
            RegisterInfo::new_phy_reg(GR32::R10D),
            RegisterInfo::new_phy_reg(GR32::R11D),
            RegisterInfo::new_phy_reg(GR32::R12D),
            RegisterInfo::new_phy_reg(GR32::R13D),
            RegisterInfo::new_phy_reg(GR32::R14D),
            RegisterInfo::new_phy_reg(GR32::R15D),
            RegisterInfo::new_phy_reg(GR64::RAX ),
            RegisterInfo::new_phy_reg(GR64::RCX ),
            RegisterInfo::new_phy_reg(GR64::RDX ),
            RegisterInfo::new_phy_reg(GR64::RBX ),
            RegisterInfo::new_phy_reg(GR64::RSP ),
            RegisterInfo::new_phy_reg(GR64::RBP ),
            RegisterInfo::new_phy_reg(GR64::RSI ),
            RegisterInfo::new_phy_reg(GR64::RDI ),
            RegisterInfo::new_phy_reg(GR64::R8  ),
            RegisterInfo::new_phy_reg(GR64::R9  ),
            RegisterInfo::new_phy_reg(GR64::R10 ),
            RegisterInfo::new_phy_reg(GR64::R11 ),
            RegisterInfo::new_phy_reg(GR64::R12 ),
            RegisterInfo::new_phy_reg(GR64::R13 ),
            RegisterInfo::new_phy_reg(GR64::R14 ),
            RegisterInfo::new_phy_reg(GR64::R15 ),
            RegisterInfo::new_phy_reg(XMM::XMM0 ),
            RegisterInfo::new_phy_reg(XMM::XMM1 ),
            RegisterInfo::new_phy_reg(XMM::XMM2 ),
            RegisterInfo::new_phy_reg(XMM::XMM3 ),
            RegisterInfo::new_phy_reg(XMM::XMM4 ),
            RegisterInfo::new_phy_reg(XMM::XMM5 ),
            RegisterInfo::new_phy_reg(XMM::XMM6 ),
            RegisterInfo::new_phy_reg(XMM::XMM7 ),
            RegisterInfo::new_phy_reg(XMM::XMM8 ),
            RegisterInfo::new_phy_reg(XMM::XMM9 ),
            RegisterInfo::new_phy_reg(XMM::XMM10),
            RegisterInfo::new_phy_reg(XMM::XMM11),
            RegisterInfo::new_phy_reg(XMM::XMM12),
            RegisterInfo::new_phy_reg(XMM::XMM13),
            RegisterInfo::new_phy_reg(XMM::XMM14),
            RegisterInfo::new_phy_reg(XMM::XMM15),
        ]
    };
}

impl MachineInst {
    pub fn debug(&self, tys: &Types, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            op.debug(tys, f)?;
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

impl MachineOperand {
    pub fn debug(&self, tys: &Types, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineOperand::Register(r) => r.fmt(f),
            MachineOperand::Constant(c) => c.fmt(f),
            MachineOperand::FrameIndex(fi) => {
                write!(f, "FI<{}, {:?}>", tys.to_string(fi.ty), fi.idx)
            }
            MachineOperand::Address(g) => g.fmt(f),
            MachineOperand::Branch(id) => write!(f, "BB#{}", id.index()),
            MachineOperand::None => write!(f, "!"),
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
            write!(f, "{:?}", op)?;
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

impl fmt::Debug for MachineConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int32(x) => write!(f, "i32 {}", x),
            Self::Int64(x) => write!(f, "i64 {}", x),
            Self::F64(x) => write!(f, "f64 {}", x),
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
