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
use crate::ir::{constant_pool::ConstantId, global_val::GlobalVariableId, types::*};
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
    pub def: Vec<RegisterOperand>,
    pub imp_use: Vec<RegisterOperand>,
    pub imp_def: Vec<RegisterOperand>,
    pub kills: Vec<RegisterOperand>,
    pub tie: FxHashMap<RegisterOperand, RegisterOperand>, // def -> use
    pub copy_for_two_addr: Option<MachineInstId>,         // id for two addr inst
    pub parent: MachineBasicBlockId,
}

#[derive(Debug, Clone)]
pub enum MachineOperand {
    // Register(RegisterId),
    Register(RegisterOperand),
    Constant(MachineConstant),
    FrameIndex(FrameIndexInfo),
    Branch(MachineBasicBlockId),
    Mem(MachineMemOperand),
    None,
}

#[derive(Clone, Eq, Hash, PartialEq, Copy)]
pub struct RegisterOperand {
    pub id: RegisterId,
    pub sub_super: Option<RegisterClassKind>,
}

#[derive(Clone)]
pub enum AddressKind {
    FunctionName(String),
    Global(GlobalVariableId),
    Constant(ConstantId),
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
                Some(rc) => vec![RegisterOperand::new(regs_info.new_virt_reg(rc))],
            },
            tie: FxHashMap::default(),
            imp_def: vec![],
            imp_use: vec![],
            kills: vec![],
            copy_for_two_addr: None,
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
            copy_for_two_addr: None,
            parent,
        }
    }

    pub fn new_with_def_reg(
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        def: Vec<RegisterOperand>,
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
            copy_for_two_addr: None,
            parent,
        }
    }

    pub fn new_with_imp_def_use(
        opcode: MachineOpcode,
        operand: Vec<MachineOperand>,
        imp_def: Vec<RegisterOperand>,
        imp_use: Vec<RegisterOperand>,
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
            copy_for_two_addr: None,
            parent,
        }
    }

    pub fn with_def(mut self, def: Vec<RegisterOperand>) -> Self {
        // At the moment this function is used, 'self.id' is None in most cases. Therefore we can't set
        // use-def information to 'def'.
        self.def = def;
        self
    }

    pub fn with_imp_use(mut self, r: RegisterOperand) -> Self {
        self.imp_use.push(r);
        self
    }

    pub fn with_imp_def(mut self, r: RegisterOperand) -> Self {
        self.imp_def.push(r);
        self
    }

    pub fn with_imp_uses(mut self, mut rs: Vec<RegisterOperand>) -> Self {
        self.imp_use.append(&mut rs);
        self
    }

    pub fn with_imp_defs(mut self, mut rs: Vec<RegisterOperand>) -> Self {
        self.imp_def.append(&mut rs);
        self
    }

    pub fn with_kills(mut self, mut rs: Vec<RegisterOperand>) -> Self {
        self.kills.append(&mut rs);
        self
    }

    pub fn copy_for_two_addr(mut self, x: Option<MachineInstId>) -> Self {
        self.copy_for_two_addr = x;
        self
    }

    pub fn set_def(&mut self, regs_info: &RegistersInfo, r: RegisterOperand) {
        regs_info.arena_ref_mut()[self.def[0].id].remove_def(self.id.unwrap());
        self.def[0] = r;
        regs_info.arena_ref_mut()[self.def[0].id].add_def(self.id.unwrap());
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
            let reg = &mut regs_info.arena_ref_mut()[reg.id];
            some_then!(id, old_id, reg.remove_use(id));
            reg.add_use(id);
        }

        for reg in &self.imp_use {
            let reg = &mut regs_info.arena_ref_mut()[reg.id];
            some_then!(id, old_id, reg.remove_use(id));
            reg.add_use(id);
        }
    }

    pub fn set_def_to_regs(&self, regs_info: &RegistersInfo, old_id: Option<MachineInstId>) {
        let id = self.id.unwrap();

        for reg in &self.def {
            let reg = &mut regs_info.arena_ref_mut()[reg.id];
            some_then!(id, old_id, reg.remove_def(id));
            reg.add_def(id);
        }

        for reg in &self.imp_def {
            let reg = &mut regs_info.arena_ref_mut()[reg.id];
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
                if r.id.kind == from.kind {
                    regs_info.arena_ref_mut()[r.id].remove_use(self.id.unwrap());
                    r.set_id(to);
                    regs_info.arena_ref_mut()[r.id].add_use(self.id.unwrap());
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
            regs_info.arena_ref_mut()[r.id].remove_use(self.id.unwrap());
        }
        for &r in to.registers() {
            regs_info.arena_ref_mut()[r.id].add_use(self.id.unwrap());
        }
        *op = to;
    }

    pub fn tie_regs(&mut self, def: RegisterOperand, use_: RegisterOperand) {
        self.tie.insert(def, use_);
    }

    pub fn set_tie(mut self, def: RegisterOperand, use_: RegisterOperand) -> Self {
        self.tie.insert(def, use_);
        self
    }

    pub fn set_tie_with_def(self, use_: RegisterOperand) -> Self {
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
        Some(self.def[0].id.as_phys_reg())
    }

    pub fn has_def_reg(&self) -> bool {
        self.def.len() > 0
    }

    pub fn get_def_reg(&self) -> Option<RegisterOperand> {
        if !self.has_def_reg() {
            return None;
        }
        Some(self.def[0])
    }

    pub fn collect_defined_regs(&self) -> Vec<&RegisterOperand> {
        let mut regs: Vec<&RegisterOperand> = self.def.iter().collect();
        regs.extend(&mut self.imp_def.iter());
        regs
    }

    pub fn collect_used_regs(&self) -> Vec<&RegisterOperand> {
        let mut regs = vec![];
        for operand in &self.operand {
            regs.append(&mut operand.registers())
        }
        for imp_use in &self.imp_use {
            regs.push(imp_use)
        }
        regs
    }

    pub fn collect_all_regs_mut(&mut self) -> Vec<&mut RegisterOperand> {
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
        MachineOperand::Register(RegisterOperand::new(regs_info.get_phys_reg(r)))
    }

    pub fn imm_i32(i: i32) -> Self {
        MachineOperand::Constant(MachineConstant::Int32(i))
    }

    pub fn registers(&self) -> Vec<&RegisterOperand> {
        match self {
            Self::Register(r) => vec![r],
            Self::Mem(mem) => mem.registers(),
            _ => vec![],
        }
    }

    pub fn registers_mut(&mut self) -> Vec<&mut RegisterOperand> {
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

    pub fn as_register(&self) -> &RegisterOperand {
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
            MachineOperand::Register(RegisterOperand { id, .. }) => id.is_virt_reg(),
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
                if let Some(sub_super) = r.sub_super {
                    Some(rc2ty(sub_super))
                } else {
                    Some(rc2ty(regs_info.arena_ref()[r.id].reg_class))
                }
            }
        }
    }
}

impl RegisterOperand {
    pub fn new(id: RegisterId) -> Self {
        Self {
            id,
            sub_super: None,
        }
    }

    pub fn set_id(&mut self, id: RegisterId) {
        self.id = id;
        self.id = self.converted_id();
    }

    pub fn sub_super(mut self, sub_super: Option<RegisterClassKind>) -> Self {
        self.sub_super = sub_super;
        self
    }

    pub fn converted_id(&self) -> RegisterId {
        match self.sub_super {
            Some(sub_super) => RegisterId {
                id: self.id.id,
                kind: match self.id.kind {
                    VirtOrPhys::Virt(v) => VirtOrPhys::Virt(v),
                    VirtOrPhys::Phys(p) => VirtOrPhys::Phys(p.reg_class_as(sub_super)),
                },
                fix: None,
            },
            None => self.id,
        }
    }
}

impl MachineMemOperand {
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

impl fmt::Debug for RegisterOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(f)?;
        if let Some(sub_super) = self.sub_super {
            write!(f, ".{:?}", sub_super)?;
        }
        Ok(())
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
            AddressKind::Constant(id) => write!(f, "const<{:?}>", id),
        }
    }
}
