use super::{module::*, opcode::*, value::*};
use id_arena::*;
use rustc_hash::FxHashSet;

pub type BasicBlockId = Id<BasicBlock>;

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub pred: Vec<BasicBlockId>,
    pub succ: Vec<BasicBlockId>,
    pub def: FxHashSet<InstructionId>,
    pub live_in: FxHashSet<InstructionId>,
    pub live_out: FxHashSet<InstructionId>,

    pub iseq: Vec<Value>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: vec![],
            pred: vec![],
            succ: vec![],
            def: FxHashSet::default(),
            live_in: FxHashSet::default(),
            live_out: FxHashSet::default(),
        }
    }

    pub fn to_string(&self, m: &Module) -> String {
        self.iseq.iter().fold("".to_string(), |s, instr| {
            format!("{}{}\n", s, instr.to_string(m, true))
        })
    }
}
