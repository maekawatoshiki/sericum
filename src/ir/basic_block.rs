use super::{module::Module, opcode::*, value::*, DumpToString};
use id_arena::*;
use rustc_hash::FxHashSet;
use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

pub type BasicBlockId = Id<BasicBlock>;

#[derive(Clone, Debug)]
pub struct BasicBlock {
    /// Information for liveness analysis
    // TODO: Will be removed
    pub liveness: Rc<RefCell<LivenessInfo>>,

    /// Predecessors
    pub pred: Vec<BasicBlockId>,

    /// Successors
    pub succ: Vec<BasicBlockId>,

    /// Instruction list
    pub iseq: Rc<RefCell<Vec<Value>>>,
}

#[derive(Clone, Debug)]
pub struct LivenessInfo {
    pub def: FxHashSet<InstructionId>,
    pub live_in: FxHashSet<InstructionId>,
    pub live_out: FxHashSet<InstructionId>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: Rc::new(RefCell::new(Vec::new())),
            pred: vec![],
            succ: vec![],
            liveness: Rc::new(RefCell::new(LivenessInfo::new())),
        }
    }

    pub fn iseq_ref<'b>(&'b self) -> Ref<Vec<Value>> {
        self.iseq.borrow()
    }

    pub fn iseq_ref_mut(&self) -> RefMut<Vec<Value>> {
        self.iseq.borrow_mut()
    }
}

impl DumpToString for BasicBlock {
    fn dump(&self, module: &Module) -> String {
        self.iseq_ref().iter().fold("".to_string(), |s, instr| {
            format!("{}{}\n", s, instr.to_string(module, true))
        })
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
