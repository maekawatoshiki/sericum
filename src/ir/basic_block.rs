use super::{module::*, value::*};
use id_arena::*;

pub type BasicBlockId = Id<BasicBlock>;

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub pred: Vec<BasicBlockId>,
    pub succ: Vec<BasicBlockId>,
    pub iseq: Vec<Value>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            iseq: vec![],
            pred: vec![],
            succ: vec![],
        }
    }

    pub fn to_string(&self, m: &Module) -> String {
        self.iseq.iter().fold("".to_string(), |s, instr| {
            format!("{}{}\n", s, instr.to_string(m, true))
        })
    }
}
