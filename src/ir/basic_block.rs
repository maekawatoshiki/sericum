use super::{module::Module, opcode::*, value::*, DumpToString};
use crate::traits::basic_block::*;
use id_arena::*;
use rustc_hash::FxHashSet;
use std::cell::{Ref, RefCell, RefMut};

pub type BasicBlockId = Id<BasicBlock>;

#[derive(Debug, Clone)]
pub struct BasicBlocks {
    pub arena: Arena<BasicBlock>,
    pub order: Vec<BasicBlockId>,
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    /// Information for liveness analysis
    // TODO: Will be removed
    pub liveness: RefCell<LivenessInfo>,

    /// Predecessors
    pub pred: FxHashSet<BasicBlockId>,

    /// Successors
    pub succ: FxHashSet<BasicBlockId>,

    /// Instruction list
    pub iseq: RefCell<Vec<Value>>,
}

#[derive(Clone, Debug)]
pub struct LivenessInfo {
    pub def: FxHashSet<InstructionId>,
    pub live_in: FxHashSet<InstructionId>,
    pub live_out: FxHashSet<InstructionId>,
}

impl BasicBlocks {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            order: vec![],
        }
    }
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: RefCell::new(Vec::new()),
            pred: FxHashSet::default(),
            succ: FxHashSet::default(),
            liveness: RefCell::new(LivenessInfo::new()),
        }
    }

    pub fn iseq_ref<'b>(&'b self) -> Ref<Vec<Value>> {
        self.iseq.borrow()
    }

    pub fn iseq_ref_mut(&self) -> RefMut<Vec<Value>> {
        self.iseq.borrow_mut()
    }

    pub fn find_inst_pos(&self, id2find: InstructionId) -> Option<usize> {
        self.iseq_ref()
            .iter()
            .enumerate()
            .find(|(_, val)| match val {
                Value::Instruction(i) if i.id == id2find => true,
                _ => false,
            })
            .map(|(i, _)| i)
    }
}

impl BasicBlockTrait for BasicBlock {
    fn get_preds(&self) -> &FxHashSet<Id<Self>> {
        &self.pred
    }

    fn get_succs(&self) -> &FxHashSet<Id<Self>> {
        &self.succ
    }
}

impl BasicBlocksTrait for BasicBlocks {
    type BB = BasicBlock;

    fn get_arena(&self) -> &Arena<Self::BB> {
        &self.arena
    }

    fn get_order(&self) -> &Vec<Id<Self::BB>> {
        &self.order
    }
}

impl DumpToString for BasicBlock {
    fn dump(&self, module: &Module) -> String {
        self.iseq_ref()
            .iter()
            .fold("".to_string(), |s, inst| {
                format!("{}{}\n", s, inst.to_string(module, true))
            })
            .trim_end()
            .to_string()
    }
}

impl LivenessInfo {
    pub fn new() -> Self {
        Self {
            def: FxHashSet::default(),
            live_in: FxHashSet::default(),
            live_out: FxHashSet::default(),
        }
    }
}
