use super::{basic_block::*, module::*, types::*, value::*};
use id_arena::*;
use std::{cell::RefCell, rc::Rc};
// use rustc_hash::FxHashMap;

pub type InstructionId = Id<Instruction>;
pub type UniqueIndex = usize; // TODO: Rename to VRegIndex?
pub type RegisterAllocInfoRef = Rc<RefCell<RegisterAllocInfo>>;

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub ty: Type,
    pub unique_idx: UniqueIndex,
    pub reg: RegisterAllocInfoRef,
}

#[derive(Debug, Clone)]
pub struct RegisterAllocInfo {
    pub reg: Option<usize>,
    pub spill: bool,
    pub last_use: Option<UniqueIndex>,
}

#[derive(Clone, Debug)]
pub enum Opcode {
    Alloca(Type),
    Load(Value),
    Store(Value, Value),
    Add(Value, Value),
    Sub(Value, Value),
    ICmp(ICmpKind, Value, Value),
    Br(BasicBlockId),
    CondBr(Value, BasicBlockId, BasicBlockId),
    Phi(Vec<(Value, BasicBlockId)>),
    Call(Value, Vec<Value>),
    Ret(Value),
}

#[derive(Clone, Debug)]
pub enum ICmpKind {
    Eq,
    Le,
    // Ne,
}

impl Instruction {
    pub fn new(opcode: Opcode, ty: Type, unique_idx: UniqueIndex) -> Self {
        Self {
            opcode,
            ty,
            unique_idx,
            reg: Rc::new(RefCell::new(RegisterAllocInfo::new())),
        }
    }

    pub fn to_string(&self, m: &Module) -> String {
        self.opcode.to_string(m)
    }
}

impl RegisterAllocInfo {
    pub fn new() -> Self {
        Self {
            reg: None,
            spill: false,
            last_use: None,
        }
    }
}

impl Opcode {
    pub fn to_string(&self, m: &Module) -> String {
        match self {
            Opcode::Alloca(ty) => format!("alloca {}", ty.to_string()),
            Opcode::Load(v) => format!("load {}", v.to_string(m, false)),
            Opcode::Store(src, dst) => format!(
                "store {}, {}",
                src.to_string(m, false),
                dst.to_string(m, false)
            ),
            Opcode::Add(v1, v2) => {
                format!("add {}, {}", v1.to_string(m, false), v2.to_string(m, false))
            }
            Opcode::Sub(v1, v2) => {
                format!("sub {}, {}", v1.to_string(m, false), v2.to_string(m, false))
            }
            Opcode::ICmp(kind, v1, v2) => format!(
                "icmp {} {}, {}",
                kind.as_str(),
                v1.to_string(m, false),
                v2.to_string(m, false)
            ),
            Opcode::Br(id) => format!("br %label.{}", id.index()),
            Opcode::CondBr(v, id1, id2) => format!(
                "br {} %label.{}, %label.{}",
                v.to_string(m, false),
                id1.index(),
                id2.index()
            ),
            Opcode::Phi(pairs) => pairs.iter().fold("phi".to_string(), |s, (val, bb)| {
                format!("{} [{}, %label.{}]", s, val.to_string(m, false), bb.index())
            }),
            Opcode::Call(v, args) => format!(
                "call {}({})",
                v.to_string(m, false),
                args.iter().fold("".to_string(), |s, val| format!(
                    "{}{}, ",
                    s,
                    val.to_string(m, false)
                ))
            ),
            Opcode::Ret(v) => format!("ret {}", v.to_string(m, false)),
        }
    }
}

impl ICmpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            ICmpKind::Eq => "eq",
            ICmpKind::Le => "le",
        }
    }
}
