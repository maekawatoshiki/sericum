use super::opcode::*;
use id_arena::*;

pub type BasicBlockId = Id<BasicBlock>;

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub iseq: Vec<Instruction>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self { iseq: vec![] }
    }

    pub fn to_string(&self) -> String {
        self.iseq.iter().fold("".to_string(), |s, instr| {
            format!("{}\n", instr.to_string())
        })
    }
}
