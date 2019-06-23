use super::{opcode::*, types::*, value::*};

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub iseq: Vec<Instruction>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self { iseq: vec![] }
    }

    pub fn build_alloca(&mut self, ty: Type) {
        self.iseq
            .push(Instruction::new(Opcode::Alloca(ty), Value::Id(0)))
    }

    pub fn to_string(&self) -> String {
        self.iseq.iter().fold("".to_string(), |mut s, instr| {
            s += &(instr.to_string() + "\n");
            s
        })
    }
}
