use super::types::{Type, Types};
use super::value::ImmediateValue;
use id_arena::{Arena, Id};
use std::fmt;

pub type ConstantId = Id<Constant>;

#[derive(Clone)]
pub struct ConstantPool {
    pub arena: Arena<Constant>,
    types: Types,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub ty: Type,
    pub kind: ConstantKind,
}

#[derive(Clone)]
pub enum ConstantKind {
    String(String),
    Array(Vec<ConstantArrayElement>),
}

#[derive(Clone)]
pub enum ConstantArrayElement {
    String(ConstantId),
    Immediate(ImmediateValue),
    Array(Vec<ConstantArrayElement>),
}

impl ConstantPool {
    pub fn new(types: Types) -> Self {
        Self {
            arena: Arena::new(),
            types,
        }
    }

    pub fn add(&mut self, c: Constant) -> ConstantId {
        self.arena.alloc(c)
    }
}

impl fmt::Debug for ConstantPool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (id, c) in &self.arena {
            writeln!(
                f,
                "@const.{} = constant {} {:?}",
                id.index(),
                self.types.to_string(c.ty),
                c.kind
            )?;
        }
        fmt::Result::Ok(())
    }
}

impl fmt::Debug for ConstantKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Array(es) => {
                write!(f, "{{")?;
                for e in es {
                    write!(f, "{:?},", e)?
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Debug for ConstantArrayElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(id) => write!(f, "@const.{}", id.index()),
            Self::Array(es) => {
                write!(f, "{{")?;
                for e in es {
                    write!(f, "{:?},", e)?
                }
                write!(f, "}}")
            }
            Self::Immediate(i) => write!(f, "{:?}", i),
        }
    }
}
