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
    ICmp(ICmpKind, Value, Value),
    Br(BasicBlockId),
    CondBr(Value, BasicBlockId, BasicBlockId),
    Phi(Vec<(Value, BasicBlockId)>),
    Ret(Value),
}

#[derive(Clone, Debug)]
pub enum ICmpKind {
    Eq,
    // Ne,
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
            Opcode::ICmp(kind, v1, v2) => format!(
                "icmp {} {}, {}",
                kind.as_str(),
                v1.to_string(f, false),
                v2.to_string(f, false)
            ),
            Opcode::Br(id) => format!("br %label.{}", id.index()),
            Opcode::CondBr(v, id1, id2) => format!(
                "br {} %label.{}, %label.{}",
                v.to_string(f, false),
                id1.index(),
                id2.index()
            ),
            Opcode::Phi(pairs) => pairs.iter().fold("phi".to_string(), |s, (val, bb)| {
                format!("{} [{}, %label.{}]", s, val.to_string(f, false), bb.index())
            }),
            Opcode::Ret(v) => format!("ret {}", v.to_string(f, false)),
        }
    }
}

impl ICmpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            ICmpKind::Eq => "eq",
        }
    }
}
