pub use super::super::inst::{TargetImmediate, TargetOpcode};
use super::super::register::{
    rc2ty, PhysReg, RegisterClassKind, RegisterId, RegistersInfo, TargetRegisterTrait, VirtReg,
    VirtRegGen, GR32, GR64, PHYS_REGISTERS_NUM, XMM,
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
pub type RegisterBaseRef = Rc<RefCell<RegisterBase>>;
pub type MachineInstId = Id<MachineInst>;

#[derive(Clone)]
pub struct MachineInst {
    pub id: Option<MachineInstId>,
    pub opcode: MachineOpcode,
    pub operand: Vec<MachineOperand>,
    pub def: Vec<RegisterId>,
    pub tie: FxHashMap<RegisterId, RegisterId>, // def -> use
    pub imp_use: Vec<RegisterId>,
    pub imp_def: Vec<RegisterId>,
    pub parent: MachineBasicBlockId,
}

#[derive(Clone, PartialEq)]
pub struct RegisterBase {
    pub vreg: VirtReg,
    pub reg: Option<PhysReg>,
    pub reg_class: RegisterClassKind,
    pub tied: Option<RegisterBaseRef>,
    pub uses: FxHashSet<MachineInstId>,
    pub defs: FxHashSet<MachineInstId>,
}

// #[derive(Clone)]
// pub struct MachineInstDef {
//     pub opcode: MachineOpcode,
//     pub operand: Vec<MachineOperand>, // reg|imm|
//     pub def: Vec<MachineRegister>,
//     pub tie: FxHashMap<MachineRegister, MachineRegister>, // def -> use
//     pub imp_use: Vec<MachineRegister>,
//     pub imp_def: Vec<MachineRegister>,
//     pub parent: MachineBasicBlockId,
// }

#[derive(Debug, Clone)]
pub enum MachineOperand {
    Register(RegisterId),
    Constant(MachineConstant),
    FrameIndex(FrameIndexInfo),
    Branch(MachineBasicBlockId),
    Mem(MachineMemOperand),
    None,
}

// TODO: target dependent
#[derive(Debug, Clone)]
pub enum MachineMemOperand {
    BaseFi(RegisterId, FrameIndexInfo),
    BaseFiOff(RegisterId, FrameIndexInfo, i32), // base, fi, off
    BaseFiAlignOff(RegisterId, FrameIndexInfo, i32, RegisterId), // base, fi, align, off
    BaseAlignOff(RegisterId, i32, RegisterId),  // base, align, off
    BaseOff(RegisterId, i32),
    Base(RegisterId),
    Address(AddressKind),
}

#[derive(Clone)]
pub enum AddressKind {
    FunctionName(String),
    Label(DataId),
}

#[derive(Clone, Copy, PartialEq)]
pub enum MachineConstant {
    Int8(i8),
    Int32(i32),
    Int64(i64),
    F64(f64),
}

#[derive(Clone, PartialEq)]
pub struct MachineRegister {
    pub info: RegisterBaseRef,
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
        // vreg_gen: &VirtRegGen,
        regs_info: &RegistersInfo,
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
                Some(rc) => vec![regs_info.new_virt_reg(rc)],
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
        def: Vec<RegisterId>,
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
        imp_def: Vec<RegisterId>,
        imp_use: Vec<RegisterId>,
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

    pub fn with_def(mut self, def: Vec<RegisterId>) -> Self {
        // At the moment this function is used, 'self.id' is None in most cases. Therefore we can't set
        // use-def information to 'def'.
        self.def = def;
        self
    }

    pub fn with_imp_use(mut self, r: RegisterId) -> Self {
        self.imp_use.push(r);
        self
    }

    pub fn with_imp_def(mut self, r: RegisterId) -> Self {
        self.imp_def.push(r);
        self
    }

    pub fn with_imp_uses(mut self, mut rs: Vec<RegisterId>) -> Self {
        self.imp_use.append(&mut rs);
        self
    }

    pub fn with_imp_defs(mut self, mut rs: Vec<RegisterId>) -> Self {
        self.imp_def.append(&mut rs);
        self
    }

    pub fn set_def(&mut self, regs_info: &RegistersInfo, r: RegisterId) {
        regs_info.arena_ref_mut()[self.def[0]].remove_def(self.id.unwrap());
        self.def[0] = r;
        regs_info.arena_ref_mut()[self.def[0]].add_def(self.id.unwrap());
    }

    pub fn set_id(&mut self, regs_info: &RegistersInfo, id: MachineInstId) {
        let old_id = self.id;
        self.id = Some(id);
        self.set_use_to_regs(regs_info, old_id);
        self.set_def_to_regs(regs_info, old_id);
    }

    pub fn set_use_to_regs(&self, regs_info: &RegistersInfo, old_id: Option<MachineInstId>) {
        let id = self.id.unwrap();

        for reg in self
            .operand
            .iter()
            .filter_map(|o| match o {
                MachineOperand::Register(_) | MachineOperand::Mem(_) => Some(o),
                _ => None,
            })
            .flat_map(|o| o.registers())
        {
            let reg = &mut regs_info.arena_ref_mut()[*reg];
            some_then!(id, old_id, reg.remove_use(id));
            reg.add_use(id);
        }

        for reg in &self.imp_use {
            let reg = &mut regs_info.arena_ref_mut()[*reg];
            some_then!(id, old_id, reg.remove_use(id));
            reg.add_use(id);
        }
    }

    pub fn set_def_to_regs(&self, regs_info: &RegistersInfo, old_id: Option<MachineInstId>) {
        let id = self.id.unwrap();

        for reg in &self.def {
            let reg = &mut regs_info.arena_ref_mut()[*reg];
            some_then!(id, old_id, reg.remove_def(id));
            reg.add_def(id);
        }

        for reg in &self.imp_def {
            let reg = &mut regs_info.arena_ref_mut()[*reg];
            some_then!(id, old_id, reg.remove_def(id));
            reg.add_def(id);
        }
    }

    pub fn replace_operand_register(
        &mut self,
        regs_info: &RegistersInfo,
        from: RegisterId,
        to: RegisterId,
    ) -> Vec<usize> {
        let mut replaced_operands_idx = vec![];
        let mut processed = FxHashSet::default();
        // TODO: This loop may run once at most
        for (i, o) in self.operand.iter_mut().enumerate() {
            let mut rs = match o {
                MachineOperand::Register(_) | MachineOperand::Mem(_) => o.registers_mut(),
                _ => continue,
            };
            for r in &mut rs {
                if !processed.insert(**r) {
                    continue;
                }
                if r.kind == from.kind {
                    regs_info.arena_ref_mut()[**r].remove_use(self.id.unwrap());
                    **r = to;
                    regs_info.arena_ref_mut()[**r].add_use(self.id.unwrap());
                    replaced_operands_idx.push(i);
                }
            }
        }
        // for r in self
        //     .operand
        //     .iter_mut()
        //     .enumerate()
        //     .filter_map(|(i, o)| {
        //         replaced_operands_idx.push(i);
        //         match o {
        //             MachineOperand::Register(_) | MachineOperand::Mem(_) => Some(o),
        //             _ => None,
        //         }
        //     })
        //     .flat_map(|o| o.registers_mut())
        //     .collect::<FxHashSet<_>>()
        //     .into_iter()
        //     .filter_map(|r| {
        //         if r.kind == from.kind {
        //             return Some(r);
        //         }
        //         None
        //     })
        // {
        //     regs_info.arena_ref_mut()[*r].remove_use(self.id.unwrap());
        //     *r = to;
        //     regs_info.arena_ref_mut()[*r].add_use(self.id.unwrap());
        // }
        replaced_operands_idx
    }

    pub fn tie_regs(&mut self, def: RegisterId, use_: RegisterId) {
        self.tie.insert(def, use_);
    }

    pub fn set_tie(mut self, def: RegisterId, use_: RegisterId) -> Self {
        self.tie.insert(def, use_);
        self
    }

    pub fn set_tie_with_def(self, use_: RegisterId) -> Self {
        let def = self.def[0];
        self.set_tie(def, use_)
    }

    // pub fn set_vreg(&self, vreg: VirtReg) {
    //     let x = &self.def[0];
    //     x.info_ref_mut().vreg = vreg;
    // }

    // pub fn set_phys_reg(&self, reg: PhysReg) {
    //     let mut reg_info = self.def[0].info_ref_mut();
    //     reg_info.reg = Some(reg);
    // }

    // pub fn get_vreg(&self) -> VirtReg {
    //     self.def[0].info_ref().vreg
    // }

    pub fn get_reg(&self) -> Option<PhysReg> {
        Some(self.def[0].as_phys_reg())
    }

    pub fn has_def_reg(&self) -> bool {
        self.def.len() > 0
    }

    pub fn get_def_reg(&self) -> Option<RegisterId> {
        if !self.has_def_reg() {
            return None;
        }
        Some(self.def[0])
    }

    pub fn collect_defined_regs(&self) -> Vec<&RegisterId> {
        let mut regs: Vec<&RegisterId> = self.def.iter().collect();
        regs.extend(&mut self.imp_def.iter());
        regs
    }

    pub fn collect_used_regs(&self) -> Vec<&RegisterId> {
        let mut regs = vec![];
        for operand in &self.operand {
            regs.append(&mut operand.registers())
        }
        for imp_use in &self.imp_use {
            regs.push(imp_use)
        }
        regs
    }

    pub fn collect_all_regs_mut(&mut self) -> Vec<&mut RegisterId> {
        let mut regs = vec![];

        for operand in &mut self.operand {
            regs.append(&mut operand.registers_mut());
        }

        for def in &mut self.def {
            regs.push(def)
        }

        for def in &mut self.imp_def {
            regs.push(def)
        }

        for use_ in &mut self.imp_use {
            regs.push(use_)
        }

        regs
    }
}

impl MachineOpcode {
    pub fn is_copy_like(&self) -> bool {
        matches!(
            self,
            MachineOpcode::MOVrr32
                | MachineOpcode::MOVrr64
                | MachineOpcode::Copy
                | MachineOpcode::MOVSDrr
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
                | MachineOpcode::JA
                | MachineOpcode::JAE
                | MachineOpcode::JBE
                | MachineOpcode::JB
        )
    }
}

impl MachineRegister {
    pub fn new(info: RegisterBaseRef) -> Self {
        Self { info }
    }

    pub fn phys_reg<T: TargetRegisterTrait>(r: T) -> Self {
        RegisterBase::phys_reg(r).into_machine_register()
    }

    pub fn add_use(&self, use_id: MachineInstId) {
        self.info_ref_mut().uses.insert(use_id);
    }

    pub fn remove_use(&self, id: MachineInstId) {
        self.info_ref_mut().uses.remove(&id);
    }

    pub fn add_def(&self, def_id: MachineInstId) {
        self.info_ref_mut().defs.insert(def_id);
    }

    pub fn remove_def(&self, id: MachineInstId) {
        self.info_ref_mut().defs.remove(&id);
    }

    pub fn copy_with_new_vreg(&self, vreg_gen: &VirtRegGen) -> Self {
        let r: RegisterBase = self.info_ref().clone();
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

    pub fn set_phys_reg(&self, reg: PhysReg) {
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

    pub fn info_ref(&self) -> Ref<RegisterBase> {
        self.info.borrow()
    }

    pub fn info_ref_mut(&self) -> RefMut<RegisterBase> {
        self.info.borrow_mut()
    }

    pub fn is_vreg(&self) -> bool {
        self.info_ref().reg.is_none()
    }

    pub fn is_phys_reg(&self) -> bool {
        self.info_ref().reg.is_some()
    }
}

impl RegisterBase {
    pub fn new(reg_class: RegisterClassKind) -> Self {
        Self {
            reg_class,
            vreg: VirtReg(0),
            reg: None,
            tied: None,
            uses: FxHashSet::default(),
            defs: FxHashSet::default(),
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
            uses: FxHashSet::default(),
            defs: FxHashSet::default(),
        }
    }

    pub fn new_ref(reg_class: RegisterClassKind) -> RegisterBaseRef {
        Rc::new(RefCell::new(Self {
            reg_class,
            vreg: VirtReg(0),
            reg: None,
            tied: None,
            uses: FxHashSet::default(),
            defs: FxHashSet::default(),
        }))
    }

    pub fn copy_with_new_vreg(&self, vreg_gen: &VirtRegGen) -> Self {
        let mut new = self.clone();
        new.vreg = vreg_gen.next_vreg();
        new
    }

    pub fn into_ref(self) -> RegisterBaseRef {
        Rc::new(RefCell::new(self))
    }

    pub fn into_machine_register(self) -> MachineRegister {
        MachineRegister::new(self.into_ref())
    }

    pub fn tie_reg(&mut self, tied: RegisterBaseRef) {
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
    pub fn phys_reg<T: TargetRegisterTrait>(regs_info: &RegistersInfo, r: T) -> MachineOperand {
        MachineOperand::Register(regs_info.get_phys_reg(r))
    }

    pub fn imm_i32(i: i32) -> Self {
        MachineOperand::Constant(MachineConstant::Int32(i))
    }

    pub fn registers(&self) -> Vec<&RegisterId> {
        match self {
            Self::Register(r) => vec![r],
            Self::Mem(mem) => mem.registers(),
            _ => vec![],
        }
    }

    pub fn registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match self {
            Self::Register(r) => vec![r],
            Self::Mem(mem) => mem.registers_mut(),
            _ => vec![],
        }
    }

    pub fn as_frame_index(&self) -> &FrameIndexInfo {
        match self {
            MachineOperand::FrameIndex(fi) => fi,
            _ => panic!(),
        }
    }

    pub fn as_register(&self) -> &RegisterId {
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

    pub fn as_mem(&self) -> &MachineMemOperand {
        match self {
            MachineOperand::Mem(mem) => mem,
            _ => panic!(),
        }
    }

    pub fn is_virtual_register(&self) -> bool {
        match self {
            MachineOperand::Register(r) => r.is_virt_reg(),
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

    pub fn get_type(&self, regs_info: &RegistersInfo) -> Option<Type> {
        match self {
            MachineOperand::Branch(_) => None,
            MachineOperand::Constant(MachineConstant::Int8(_)) => Some(Type::Int8),
            MachineOperand::Constant(MachineConstant::Int32(_)) => Some(Type::Int32),
            MachineOperand::Constant(MachineConstant::Int64(_)) => Some(Type::Int64),
            MachineOperand::Constant(MachineConstant::F64(_)) => Some(Type::F64),
            MachineOperand::FrameIndex(fi) => Some(fi.ty.clone()),
            MachineOperand::Mem(_) => None,
            MachineOperand::None => None, // TODO
            MachineOperand::Register(r) => Some(rc2ty(regs_info.arena_ref()[*r].reg_class)),
        }
    }
}

impl MachineMemOperand {
    pub fn registers(&self) -> Vec<&RegisterId> {
        match self {
            MachineMemOperand::BaseFi(r, _)
            | MachineMemOperand::BaseFiOff(r, _, _)
            | MachineMemOperand::BaseOff(r, _)
            | MachineMemOperand::Base(r) => vec![r],
            MachineMemOperand::BaseAlignOff(r, _, r2)
            | MachineMemOperand::BaseFiAlignOff(r, _, _, r2) => vec![r, r2],
            MachineMemOperand::Address(_) => vec![],
        }
    }

    pub fn registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match self {
            MachineMemOperand::BaseFi(r, _)
            | MachineMemOperand::BaseFiOff(r, _, _)
            | MachineMemOperand::BaseOff(r, _)
            | MachineMemOperand::Base(r) => vec![r],
            MachineMemOperand::BaseAlignOff(r, _, r2)
            | MachineMemOperand::BaseFiAlignOff(r, _, _, r2) => vec![r, r2],
            MachineMemOperand::Address(_) => vec![],
        }
    }

    pub fn as_address(&self) -> &AddressKind {
        match self {
            Self::Address(kind) => kind,
            _ => panic!(),
        }
    }
}

impl AddressKind {
    pub fn as_label(&self) -> DataId {
        match self {
            AddressKind::Label(id) => *id,
            _ => panic!(),
        }
    }
}

impl MachineConstant {
    pub fn size_in_byte(&self) -> usize {
        match self {
            MachineConstant::Int8(_) => 1,
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
    pub fn as_i8(&self) -> i8 {
        match self {
            Self::Int8(i) => *i,
            _ => panic!(),
        }
    }

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
    pub static PHYS_REGS: [RegisterBase; PHYS_REGISTERS_NUM] = {
        [
            RegisterBase::new_phy_reg(GR32::EAX ),
            RegisterBase::new_phy_reg(GR32::ECX ),
            RegisterBase::new_phy_reg(GR32::EDX ),
            RegisterBase::new_phy_reg(GR32::EBX ),
            RegisterBase::new_phy_reg(GR32::ESP ),
            RegisterBase::new_phy_reg(GR32::EBP ),
            RegisterBase::new_phy_reg(GR32::ESI ),
            RegisterBase::new_phy_reg(GR32::EDI ),
            RegisterBase::new_phy_reg(GR32::R8D ),
            RegisterBase::new_phy_reg(GR32::R9D ),
            RegisterBase::new_phy_reg(GR32::R10D),
            RegisterBase::new_phy_reg(GR32::R11D),
            RegisterBase::new_phy_reg(GR32::R12D),
            RegisterBase::new_phy_reg(GR32::R13D),
            RegisterBase::new_phy_reg(GR32::R14D),
            RegisterBase::new_phy_reg(GR32::R15D),
            RegisterBase::new_phy_reg(GR64::RAX ),
            RegisterBase::new_phy_reg(GR64::RCX ),
            RegisterBase::new_phy_reg(GR64::RDX ),
            RegisterBase::new_phy_reg(GR64::RBX ),
            RegisterBase::new_phy_reg(GR64::RSP ),
            RegisterBase::new_phy_reg(GR64::RBP ),
            RegisterBase::new_phy_reg(GR64::RSI ),
            RegisterBase::new_phy_reg(GR64::RDI ),
            RegisterBase::new_phy_reg(GR64::R8  ),
            RegisterBase::new_phy_reg(GR64::R9  ),
            RegisterBase::new_phy_reg(GR64::R10 ),
            RegisterBase::new_phy_reg(GR64::R11 ),
            RegisterBase::new_phy_reg(GR64::R12 ),
            RegisterBase::new_phy_reg(GR64::R13 ),
            RegisterBase::new_phy_reg(GR64::R14 ),
            RegisterBase::new_phy_reg(GR64::R15 ),
            RegisterBase::new_phy_reg(XMM::XMM0 ),
            RegisterBase::new_phy_reg(XMM::XMM1 ),
            RegisterBase::new_phy_reg(XMM::XMM2 ),
            RegisterBase::new_phy_reg(XMM::XMM3 ),
            RegisterBase::new_phy_reg(XMM::XMM4 ),
            RegisterBase::new_phy_reg(XMM::XMM5 ),
            RegisterBase::new_phy_reg(XMM::XMM6 ),
            RegisterBase::new_phy_reg(XMM::XMM7 ),
            RegisterBase::new_phy_reg(XMM::XMM8 ),
            RegisterBase::new_phy_reg(XMM::XMM9 ),
            RegisterBase::new_phy_reg(XMM::XMM10),
            RegisterBase::new_phy_reg(XMM::XMM11),
            RegisterBase::new_phy_reg(XMM::XMM12),
            RegisterBase::new_phy_reg(XMM::XMM13),
            RegisterBase::new_phy_reg(XMM::XMM14),
            RegisterBase::new_phy_reg(XMM::XMM15),
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
            MachineOperand::Branch(id) => write!(f, "BB#{}", id.index()),
            MachineOperand::Mem(mem) => write!(f, "{:?}", mem),
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
            Self::Int8(x) => write!(f, "i8 {}", x),
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

impl fmt::Debug for RegisterBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.reg {
            Some(phy_reg) => phy_reg.fmt(f),
            None => self.vreg.fmt(f),
        }
    }
}

impl fmt::Debug for AddressKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AddressKind::FunctionName(name) => write!(f, "addr<fn:{}>", name),
            AddressKind::Label(id) => write!(f, "label<{}>", id),
        }
    }
}
