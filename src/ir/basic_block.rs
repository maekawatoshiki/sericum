use super::opcode::*;

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub iseq: Vec<Opcode>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self { iseq: vec![] }
    }
}
