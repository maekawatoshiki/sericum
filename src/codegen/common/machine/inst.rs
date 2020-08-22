use crate::codegen::arch::machine::{
    frame_object::*,
    inst::MachineMemOperand,
    inst_def::TargetOpcode,
    register::{
        rc2ty, PhysReg, RegisterClassKind, RegisterId, RegistersInfo, TargetRegisterTrait,
        VirtOrPhys,
    },
};
use crate::codegen::common::machine::{basic_block::*, const_data::DataId};
use crate::ir::{global_val::GlobalVariableId, types::*};
use id_arena::*;
use rustc_hash::FxHashMap;
use std::{fmt, fmt::Debug};

pub type MachineOpcode = TargetOpcode;
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
    pub kills: Vec<RegisterId>,
    pub parent: MachineBasicBlockId,
}

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
// #[derive(Debug, Clone)]
// pub enum MachineMemOperand {
//     BaseFi(RegisterId, FrameIndexInfo),
//     BaseFiOff(RegisterId, FrameIndexInfo, i32), // base, fi, off
//     BaseFiAlignOff(RegisterId, FrameIndexInfo, i32, RegisterId), // base, fi, align, off
//     BaseAlignOff(RegisterId, i32, RegisterId),  // base, align, off
//     BaseOff(RegisterId, i32),
//     Base(RegisterId),
//     Address(AddressKind),
// }

#[derive(Clone)]
pub enum AddressKind {
    FunctionName(String),
    Global(GlobalVariableId),
    Label(DataId),
}

#[derive(Clone, Copy, PartialEq)]
pub enum MachineConstant {
    Int8(i8),
    Int32(i32),
    Int64(i64),
    F64(f64),
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
            kills: vec![],
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
            kills: vec![],
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
            kills: vec![],
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
            kills: vec![],
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

    pub fn with_kills(mut self, mut rs: Vec<RegisterId>) -> Self {
        self.kills.append(&mut rs);
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

        for reg in self.operand.iter().flat_map(|o| o.registers()) {
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

    pub fn replace_operand_block(&mut self, from: MachineBasicBlockId, to: MachineBasicBlockId) {
        for o in &mut self.operand {
            match o {
                MachineOperand::Branch(bb) if bb == &from => *bb = to,
                _ => continue,
            };
        }
    }

    pub fn replace_operand_register(
        &mut self,
        regs_info: &RegistersInfo,
        from: RegisterId,
        to: RegisterId,
    ) -> Vec<usize> {
        let mut replaced_operands_idx = vec![];
        // TODO: This loop may run once at most
        for (i, o) in self.operand.iter_mut().enumerate() {
            for r in &mut o.registers_mut() {
                if r.kind == from.kind {
                    let fix = r.fix;
                    regs_info.arena_ref_mut()[**r].remove_use(self.id.unwrap());
                    **r = to;
                    r.fix = fix;
                    if r.is_phys_reg() {
                        r.kind = VirtOrPhys::Phys(r.as_fixed_phys_reg());
                        r.fix = None;
                    }
                    regs_info.arena_ref_mut()[**r].add_use(self.id.unwrap());
                    replaced_operands_idx.push(i);
                }
            }
        }
        replaced_operands_idx
    }

    pub fn replace_nth_operand_with(
        &mut self,
        regs_info: &RegistersInfo,
        nth: usize,
        to: MachineOperand,
    ) {
        let op = &mut self.operand[nth];
        for &r in op.registers() {
            regs_info.arena_ref_mut()[r].remove_use(self.id.unwrap());
        }
        for &r in to.registers() {
            regs_info.arena_ref_mut()[r].add_use(self.id.unwrap());
        }
        *op = to;
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

    pub fn is_basic_block(&self) -> bool {
        matches!(self, MachineOperand::Branch(_))
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
            MachineOperand::Constant(MachineConstant::Int8(_)) => Some(Type::i8),
            MachineOperand::Constant(MachineConstant::Int32(_)) => Some(Type::i32),
            MachineOperand::Constant(MachineConstant::Int64(_)) => Some(Type::i64),
            MachineOperand::Constant(MachineConstant::F64(_)) => Some(Type::f64),
            MachineOperand::FrameIndex(fi) => Some(fi.ty),
            MachineOperand::Mem(mem) => mem.get_type(),
            MachineOperand::None => None, // TODO
            MachineOperand::Register(r) => {
                if let Some(fix) = r.fix {
                    Some(rc2ty(fix))
                } else {
                    Some(rc2ty(regs_info.arena_ref()[*r].reg_class))
                }
            }
        }
    }
}

impl MachineMemOperand {
    //     pub fn registers(&self) -> Vec<&RegisterId> {
    //         match self {
    //             MachineMemOperand::BaseFi(r, _)
    //             | MachineMemOperand::BaseFiOff(r, _, _)
    //             | MachineMemOperand::BaseOff(r, _)
    //             | MachineMemOperand::Base(r) => vec![r],
    //             MachineMemOperand::BaseAlignOff(r, _, r2)
    //             | MachineMemOperand::BaseFiAlignOff(r, _, _, r2) => vec![r, r2],
    //             MachineMemOperand::Address(_) => vec![],
    //         }
    //     }
    //
    //     pub fn registers_mut(&mut self) -> Vec<&mut RegisterId> {
    //         match self {
    //             MachineMemOperand::BaseFi(r, _)
    //             | MachineMemOperand::BaseFiOff(r, _, _)
    //             | MachineMemOperand::BaseOff(r, _)
    //             | MachineMemOperand::Base(r) => vec![r],
    //             MachineMemOperand::BaseAlignOff(r, _, r2)
    //             | MachineMemOperand::BaseFiAlignOff(r, _, _, r2) => vec![r, r2],
    //             MachineMemOperand::Address(_) => vec![],
    //         }
    //     }
    //
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

    pub fn as_global(&self) -> &GlobalVariableId {
        match self {
            AddressKind::Global(id) => id,
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

impl MachineOpcode {
    pub fn is_jmp(&self) -> bool {
        self.is_unconditional_jmp() | self.is_conditional_jmp()
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

impl fmt::Debug for AddressKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AddressKind::FunctionName(name) => write!(f, "addr<fn:{}>", name),
            AddressKind::Label(id) => write!(f, "label<{}>", id),
            AddressKind::Global(id) => write!(f, "global<{:?}>", id),
        }
    }
}
