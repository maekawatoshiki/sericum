use super::{basic_block::*, function::*, types::*, value::*};
use id_arena::*;

pub type InstructionId = Id<Instruction>;

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Opcode {
    Alloca(Type),
    Load(Value),
    Add(Value, Value),
    Br(BasicBlockId),
    Ret(Value),
}

impl Instruction {
    pub fn new(opcode: Opcode, ty: Type) -> Self {
        Self { opcode, ty }
    }

    pub fn to_string(&self, f: &Function) -> String {
        self.opcode.to_string(f)
    }
}

impl Opcode {
    pub fn to_string(&self, f: &Function) -> String {
        match self {
            Opcode::Alloca(ty) => format!("alloca {}", ty.to_string()),
            Opcode::Load(v) => format!("load {}", v.to_string(f, false)),
            Opcode::Add(v1, v2) => {
                format!("add {}, {}", v1.to_string(f, false), v2.to_string(f, false))
            }
            Opcode::Br(id) => format!("br label{}", id.index()),
            Opcode::Ret(v) => format!("ret {}", v.to_string(f, false)),
        }
    }
}
