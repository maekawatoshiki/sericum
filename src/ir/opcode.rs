use super::{basic_block::BasicBlockId, module::Module, types::*, value::*};
use id_arena::{Arena, Id};
use std::cell::RefCell;

pub type InstructionId = Id<Instruction>;
pub type VirtualRegister = usize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Register(usize);

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: Vec<Operand>,
    pub ty: Type,
    pub id: Option<InstructionId>,
    pub parent: BasicBlockId,
    pub uses: RefCell<Vec<InstructionId>>,
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Opcode {
    Alloca,
    Load,
    Store,
    GetElementPtr, // ptr val, indices
    Add,
    Sub,
    Mul,
    Rem,
    ICmp,
    Br,
    CondBr,
    Phi,
    Call,
    Ret,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Type(Type),
    Value(Value),
    BasicBlock(BasicBlockId),
    ICmpKind(ICmpKind),
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum ICmpKind {
    Eq,
    Le,
    Lt,
    // Ne,
}

impl Instruction {
    pub fn new(opcode: Opcode, operands: Vec<Operand>, ty: Type, parent: BasicBlockId) -> Self {
        Self {
            opcode,
            operands,
            ty,
            id: None,
            parent,
            uses: RefCell::new(vec![]),
        }
    }

    pub fn set_id(&mut self, id: InstructionId) {
        self.id = Some(id);
    }

    pub fn set_uses(&self, inst_arena: &Arena<Instruction>) {
        for operand in &self.operands {
            match operand {
                Operand::Value(Value::Instruction(InstructionValue { id, .. })) => {
                    inst_arena[*id].uses.borrow_mut().push(self.id.unwrap());
                }
                _ => {}
            }
        }
    }

    pub fn to_string(&self, parent: &Module) -> String {
        let mut output = self.opcode.to_string().to_owned();
        for (i, operand) in self.operands.iter().enumerate() {
            output = format!(
                "{}{}{}",
                output,
                if i == 0 { " " } else { ", " },
                operand.to_string(parent)
            );
        }
        format!(
            "{} ({}, {:?})",
            output,
            self.id.unwrap().index(),
            self.uses.borrow()
        )
    }
}

impl Opcode {
    pub fn returns_value(&self) -> bool {
        match self {
            Opcode::Br | Opcode::CondBr | Opcode::Ret | Opcode::Store | Opcode::Call|
                /* alloca doesn't return value = */ Opcode::Alloca => false,
            _ => true,
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            Opcode::Alloca => "alloca",
            Opcode::Load => "load",
            Opcode::Store => "store",
            Opcode::GetElementPtr => "getelementptr",
            Opcode::Add => "add",
            Opcode::Sub => "sub",
            Opcode::Mul => "mul",
            Opcode::Rem => "rem",
            Opcode::ICmp => "icmp",
            Opcode::Br => "br",
            Opcode::CondBr => "br",
            Opcode::Phi => "phi",
            Opcode::Call => "call",
            Opcode::Ret => "ret",
        }
    }
}

impl Operand {
    // TODO: should return cow?
    pub fn to_string(&self, parent: &Module) -> String {
        match self {
            Self::BasicBlock(id) => format!("%label.{}", id.index()),
            Self::ICmpKind(kind) => kind.as_str().to_owned(),
            Self::Type(ty) => parent.types.to_string(*ty),
            Self::Value(v) => v.to_string(parent, false),
        }
    }

    pub fn as_basic_block(&self) -> &BasicBlockId {
        match self {
            Self::BasicBlock(id) => id,
            _ => panic!(),
        }
    }
    pub fn as_icmp_kind(&self) -> &ICmpKind {
        match self {
            Self::ICmpKind(kind) => kind,
            _ => panic!(),
        }
    }
    pub fn as_type(&self) -> &Type {
        match self {
            Self::Type(ty) => ty,
            _ => panic!(),
        }
    }
    pub fn as_value(&self) -> &Value {
        match self {
            Self::Value(v) => v,
            _ => panic!(),
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
