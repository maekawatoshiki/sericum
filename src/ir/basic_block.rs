use super::opcode::*;

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub iseq: Vec<Opcode>,
}
