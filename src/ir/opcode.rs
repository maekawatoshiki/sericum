use super::types::*;
use super::value::*;

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub value: Value,
}

#[derive(Clone, Debug)]
pub enum Opcode {
    Alloca(Type),
    Load(Value),
    Add(Value, Value),
    Ret(Value),
}

impl Instruction {
    pub fn new(opcode: Opcode, value: Value) -> Self {
        Self { opcode, value }
    }

    pub fn to_string(&self) -> String {
        (match self.value {
            Value::Id(id, _) => format!("%{} = ", id),
            Value::None => "".to_string(),
            _ => unreachable!(),
        }) + self.opcode.to_string().as_str()
    }
}

impl Opcode {
    pub fn to_string(&self) -> String {
        match self {
            Opcode::Alloca(ty) => format!("alloca {}", ty.to_string()),
            Opcode::Load(v) => format!("load {}", v.to_string()),
            Opcode::Add(v1, v2) => format!("add {}, {}", v1.to_string(), v2.to_string()),
            Opcode::Ret(v) => format!("ret {}", v.to_string()),
        }
    }
}
