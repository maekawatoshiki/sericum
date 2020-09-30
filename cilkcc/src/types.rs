use id_arena::{Arena, Id};
use std::ops::{Index, IndexMut};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Sign {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StorageClass {
    Typedef,
    Extern,
    Static,
    Auto,
    Register,
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum Type {
    Void,
    Char(Sign),
    Short(Sign),
    Int(Sign),
    Long(Sign),
    LLong(Sign),
    Float,
    Double,
    Pointer(CompoundTypeId),
    // Array(Box<Type>, i32),               // ary elem type, size
    Func(CompoundTypeId), // return type, param types, vararg
                          // Struct(RectypeName, Vec<AST>),       // name, fields
                          // Union(RectypeName, Vec<AST>, usize), // name, fields, means size of nth field is size of the union
                          // Enum,                                // as same as Int
}

pub enum CompoundType {
    Pointer {
        inner: Type,
    },
    Array {
        inner: Type,
        len: i32,
    },
    Func {
        ret: Type,
        params: Vec<Type>,
        vararg: bool,
    },
    Struct {
        name: String,
        fields: Vec<Type>,
    },
    Union {
        name: String,
        fields: Vec<Type>,
    },
    Enum, // == i32
}

pub struct CompoundTypes(Arena<CompoundType>);

pub type CompoundTypeId = Id<CompoundType>;

impl CompoundTypes {
    pub fn new() -> Self {
        CompoundTypes(Arena::new())
    }

    pub fn pointer(&mut self, inner: Type) -> Type {
        Type::Pointer(
            match self.0.iter().find_map(|(id, t)| match t {
                CompoundType::Pointer { inner: inner2 } if &inner == inner2 => Some(id),
                _ => None,
            }) {
                Some(id) => id,
                None => self.0.alloc(CompoundType::Pointer { inner }),
            },
        )
    }

    pub fn func(&mut self, ret: Type, params: Vec<Type>, vararg: bool) -> Type {
        Type::Func(
            match self.0.iter().find_map(|(id, t)| match t {
                CompoundType::Func {
                    ret: r,
                    params: p,
                    vararg: v,
                } if r == &ret && p == &params && v == &vararg => Some(id),
                _ => None,
            }) {
                Some(id) => id,
                None => self.0.alloc(CompoundType::Func {
                    ret,
                    params,
                    vararg,
                }),
            },
        )
    }
}

impl Index<CompoundTypeId> for CompoundTypes {
    type Output = CompoundType;

    fn index(&self, id: CompoundTypeId) -> &Self::Output {
        &self.0[id]
    }
}

impl IndexMut<CompoundTypeId> for CompoundTypes {
    fn index_mut(&mut self, id: CompoundTypeId) -> &mut Self::Output {
        &mut self.0[id]
    }
}
