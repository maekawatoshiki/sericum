// use super::{module::*, opcode::*, value::*};
use super::super::register::{
    PhysRegSet, RegisterId, TargetRegisterTrait, VirtOrPhys, CALLEE_SAVED_REGS,
};
use super::inst::*;
use crate::traits::basic_block::*;
use id_arena::*;
use rustc_hash::FxHashSet;
use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

pub type MachineBasicBlockId = Id<MachineBasicBlock>;

#[derive(Debug, Clone)]
pub struct MachineBasicBlocks {
    pub arena: Arena<MachineBasicBlock>,
    pub order: Vec<MachineBasicBlockId>,
}

#[derive(Clone, Debug)]
pub struct MachineBasicBlock {
    /// Information for liveness analysis
    pub liveness: Rc<RefCell<LivenessInfo>>,

    /// Predecessors
    pub pred: Vec<MachineBasicBlockId>,

    /// Successors
    pub succ: Vec<MachineBasicBlockId>,

    /// Instruction list
    pub iseq: RefCell<Vec<MachineInstId>>,
}

#[derive(Clone, Debug)]
pub struct LivenessInfo {
    pub phys_def: PhysRegSet,
    pub def: FxHashSet<RegisterId>,
    pub live_in: FxHashSet<RegisterId>,
    pub live_out: FxHashSet<RegisterId>,
}

impl BasicBlockTrait for MachineBasicBlock {
    fn get_preds(&self) -> &Vec<Id<Self>> {
        &self.pred
    }

    fn get_succs(&self) -> &Vec<Id<Self>> {
        &self.succ
    }
}

impl BasicBlocksTrait for MachineBasicBlocks {
    type BB = MachineBasicBlock;

    fn get_arena(&self) -> &Arena<Self::BB> {
        &self.arena
    }

    fn get_order(&self) -> &Vec<Id<Self::BB>> {
        &self.order
    }
}

impl MachineBasicBlocks {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            order: vec![],
        }
    }

    pub fn id_and_block(&self) -> BasicBlocksIter<MachineBasicBlocks> {
        BasicBlocksIter::new(self)
    }

    pub fn get_def_phys_regs(&self) -> PhysRegSet {
        let mut set = PhysRegSet::new();
        for (_id, block) in self.id_and_block() {
            set = set | block.liveness_ref().phys_def.clone();
        }
        set
    }
}

impl MachineBasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: RefCell::new(vec![]),
            pred: vec![],
            succ: vec![],
            liveness: Rc::new(RefCell::new(LivenessInfo::new())),
        }
    }

    pub fn iseq_ref(&self) -> Ref<Vec<MachineInstId>> {
        self.iseq.borrow()
    }

    pub fn iseq_ref_mut(&self) -> RefMut<Vec<MachineInstId>> {
        self.iseq.borrow_mut()
    }

    pub fn find_inst_pos(&self, id2find: MachineInstId) -> Option<usize> {
        self.iseq_ref()
            .iter()
            .enumerate()
            .find(|(_, id)| *id == &id2find)
            .map(|(i, _)| i)
    }

    pub fn liveness_ref(&self) -> Ref<LivenessInfo> {
        self.liveness.borrow()
    }

    pub fn liveness_ref_mut(&self) -> RefMut<LivenessInfo> {
        self.liveness.borrow_mut()
    }
}

impl LivenessInfo {
    pub fn new() -> Self {
        Self {
            phys_def: PhysRegSet::new(),
            def: FxHashSet::default(),
            live_in: FxHashSet::default(),
            live_out: FxHashSet::default(),
        }
    }

    pub fn add_phys_def<T: TargetRegisterTrait>(&mut self, r: T) {
        self.phys_def.unite(&r.regs_sharing_same_register_file());
    }

    /// Add def.
    /// If ``self.def`` has ``reg``'s super physical register, we do't add.
    /// If ``self.def`` has ``reg``'s sub physical register, remove it and add ``reg``.
    pub fn add_def(&mut self, reg: RegisterId) -> bool {
        if reg.is_virt_reg() {
            return self.def.insert(reg);
        }

        let p = reg.as_phys_reg();
        let mut cur = p;

        if CALLEE_SAVED_REGS.with(|regs| regs.has(p)) {
            return false;
        }

        if self.def.contains(&reg) {
            return false;
        }

        while let Some(sub) = cur.sub_reg() {
            cur = sub;
            let key = RegisterId {
                id: reg.id,
                kind: VirtOrPhys::Phys(cur),
            };
            if self.def.contains(&key) {
                self.def.remove(&key);
                self.def.insert(reg);
                return false;
            }
        }

        cur = p;

        while let Some(sub) = cur.super_reg() {
            cur = sub;
            let key = RegisterId {
                id: reg.id,
                kind: VirtOrPhys::Phys(cur),
            };
            if self.def.contains(&key) {
                return false;
            }
        }

        self.def.insert(reg)
    }

    /// Add livein.
    /// If ``self.live_in`` has ``reg``'s super physical register, we do't add.
    /// If ``self.live_in`` has ``reg``'s sub physical register, remove it and add ``reg``.
    pub fn add_live_in(&mut self, reg: RegisterId) -> bool {
        if reg.is_virt_reg() {
            return self.live_in.insert(reg);
        }

        let p = reg.as_phys_reg();
        let mut cur = p;

        if CALLEE_SAVED_REGS.with(|regs| regs.has(p)) {
            return false;
        }

        if self.live_in.contains(&reg) {
            return false;
        }

        while let Some(sub) = cur.sub_reg() {
            cur = sub;
            let key = RegisterId {
                id: reg.id,
                kind: VirtOrPhys::Phys(cur),
            };
            if self.live_in.contains(&key) {
                self.live_in.remove(&key);
                self.live_in.insert(reg);
                return false;
            }
        }

        cur = p;

        while let Some(sub) = cur.super_reg() {
            cur = sub;
            let key = RegisterId {
                id: reg.id,
                kind: VirtOrPhys::Phys(cur),
            };
            if self.live_in.contains(&key) {
                return false;
            }
        }

        self.live_in.insert(reg)
    }

    /// Add liveout.
    /// If ``self.live_out`` has ``reg``'s super physical register, we do't add.
    /// If ``self.live_out`` has ``reg``'s sub physical register, remove it and add ``reg``.
    pub fn add_live_out(&mut self, reg: RegisterId) -> bool {
        if reg.is_virt_reg() {
            return self.live_out.insert(reg);
        }

        let p = reg.as_phys_reg();
        let mut cur = p;

        if CALLEE_SAVED_REGS.with(|regs| regs.has(p)) {
            return false;
        }

        if self.live_out.contains(&reg) {
            return false;
        }

        while let Some(sub) = cur.sub_reg() {
            cur = sub;
            let key = RegisterId {
                id: reg.id,
                kind: VirtOrPhys::Phys(cur),
            };
            if self.live_out.contains(&key) {
                self.live_out.remove(&key);
                self.live_out.insert(reg);
                return false;
            }
        }

        cur = p;

        while let Some(sub) = cur.super_reg() {
            cur = sub;
            let key = RegisterId {
                id: reg.id,
                kind: VirtOrPhys::Phys(cur),
            };
            if self.live_out.contains(&key) {
                return false;
            }
        }

        self.live_out.insert(reg)
    }

    pub fn clear(&mut self) {
        self.phys_def = PhysRegSet::new();
        self.def.clear();
        self.live_in.clear();
        self.live_out.clear();
    }
}
