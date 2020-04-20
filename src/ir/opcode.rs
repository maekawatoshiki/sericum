use super::{basic_block::BasicBlockId, module::Module, types::*, value::*};
use id_arena::Id;
use std::cell::RefCell;

pub type InstructionId = Id<Instruction>;
pub type VirtualRegister = usize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Register(usize);

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub ty: Type,
    pub id: Option<InstructionId>,
    pub parent: BasicBlockId,
    pub uses: RefCell<Vec<InstructionId>>,
}

#[derive(Clone, Debug)]
pub enum Opcode {
    Alloca(Type),
    Load(Value),
    Store(Value, Value),
    GetElementPtr(Value, Vec<Value>), // ptr val, indices
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Rem(Value, Value),
    ICmp(ICmpKind, Value, Value),
    Br(BasicBlockId),
    CondBr(Value, BasicBlockId, BasicBlockId),
    Phi(Vec<(Value, BasicBlockId)>),
    Call(Value, Vec<Value>),
    Ret(Value),
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum ICmpKind {
    Eq,
    Le,
    Lt,
    // Ne,
}

impl Instruction {
    pub fn new(opcode: Opcode, ty: Type, parent: BasicBlockId) -> Self {
        Self {
            opcode,
            ty,
            id: None,
            parent,
            uses: RefCell::new(vec![]),
        }
    }

    pub fn set_id(&mut self, id: InstructionId) {
        self.id = Some(id);
    }

    // pub fn set_uses(&mut self, inst_arena: &Arena<Instruction>) {
    // }

    pub fn to_string(&self, parent: &Module) -> String {
        self.opcode.to_string(parent)
    }
}

impl Opcode {
    pub fn returns_value(&self) -> bool {
        match self {
            Opcode::Br(_) | Opcode::CondBr(_, _, _) | Opcode::Ret(_) | Opcode::Store(_, _) | Opcode::Call(_, _) |
                /* alloca doesn't return value = */ Opcode::Alloca(_) => false,
            _ => true,
        }
    }

    pub fn to_string(&self, parent: &Module) -> String {
        match self {
            Opcode::Alloca(ty) => format!("alloca {}", parent.types.to_string(*ty)),
            Opcode::Load(v) => format!("load {}", v.to_string(parent, false)),
            Opcode::Store(src, dst) => format!(
                "store {}, {}",
                src.to_string(parent, false),
                dst.to_string(parent, false)
            ),
            Opcode::GetElementPtr(ptrval, indices) => format!(
                "getelementptr {}{}",
                ptrval.to_string(parent, false),
                indices.iter().fold("".to_string(), |mut s, idx| {
                    s += ", ";
                    s += idx.to_string(parent, false).as_str();
                    s
                })
            ),
            Opcode::Add(v1, v2) => format!(
                "add {}, {}",
                v1.to_string(parent, false),
                v2.to_string(parent, false)
            ),
            Opcode::Sub(v1, v2) => format!(
                "sub {}, {}",
                v1.to_string(parent, false),
                v2.to_string(parent, false)
            ),
            Opcode::Mul(v1, v2) => format!(
                "mul {}, {}",
                v1.to_string(parent, false),
                v2.to_string(parent, false)
            ),
            Opcode::Rem(v1, v2) => format!(
                "rem {}, {}",
                v1.to_string(parent, false),
                v2.to_string(parent, false)
            ),
            Opcode::ICmp(kind, v1, v2) => format!(
                "icmp {} {}, {}",
                kind.as_str(),
                v1.to_string(parent, false),
                v2.to_string(parent, false)
            ),
            Opcode::Br(id) => format!("br %label.{}", id.index()),
            Opcode::CondBr(v, id1, id2) => format!(
                "br {} %label.{}, %label.{}",
                v.to_string(parent, false),
                id1.index(),
                id2.index()
            ),
            Opcode::Phi(pairs) => pairs.iter().fold("phi".to_string(), |s, (val, bb)| {
                format!(
                    "{} [{}, %label.{}]",
                    s,
                    val.to_string(parent, false),
                    bb.index()
                )
            }),
            Opcode::Call(v, args) => format!(
                "call {}({})",
                v.to_string(parent, false),
                args.iter().fold("".to_string(), |s, val| format!(
                    "{}{}, ",
                    s,
                    val.to_string(parent, false)
                ))
            ),
            Opcode::Ret(v) => format!("ret {}", v.to_string(parent, false)),
        }
    }
}

impl ICmpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            ICmpKind::Eq => "eq",
            ICmpKind::Lt => "lt",
            ICmpKind::Le => "le",
        }
    }
}

impl Register {
    pub fn shift(self, n: usize) -> Register {
        Register(self.0 + n)
    }

    pub fn as_u8(&self) -> u8 {
        self.0 as u8
    }
}
