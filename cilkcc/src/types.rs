use cilk::types;
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
    Array(CompoundTypeId),
    Func(CompoundTypeId), // return type, param types, vararg
    Struct(CompoundTypeId),
    Union(CompoundTypeId),
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

    pub fn array(&mut self, inner: Type, len: i32) -> Type {
        Type::Array(
            match self.0.iter().find_map(|(id, t)| match t {
                CompoundType::Array { inner: i, len: l } if i == &inner && l == &len => Some(id),
                _ => None,
            }) {
                Some(id) => id,
                None => self.0.alloc(CompoundType::Array { inner, len }),
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

impl CompoundType {
    pub fn as_pointer(&self) -> Type {
        match self {
            Self::Pointer { inner } => *inner,
            _ => panic!(),
        }
    }

    pub fn as_array(&self) -> (Type, i32) {
        match self {
            Self::Array { inner, len } => (*inner, *len),
            _ => panic!(),
        }
    }

    pub fn as_func(&self) -> (Type, &Vec<Type>, bool) {
        match self {
            Self::Func {
                ret,
                params,
                vararg,
            } => (*ret, params, *vararg),
            _ => panic!(),
        }
    }

    pub fn as_struct(&self) -> (&String, &Vec<Type>) {
        match self {
            Self::Struct { name, fields } => (name, fields),
            _ => panic!(),
        }
    }
}

pub trait TypeConversion<T> {
    fn conv(&self, compound_types: &CompoundTypes, types: &types::Types) -> cilk::types::Type;
}

impl TypeConversion<types::Type> for Type {
    fn conv(&self, compound_types: &CompoundTypes, types: &types::Types) -> cilk::types::Type {
        match self {
            Type::Void => types::Type::Void,
            Type::Char(_) => types::Type::i8,
            Type::Short(_) => panic!(),
            Type::Int(_) => types::Type::i32,
            Type::Long(_) => types::Type::i64,
            Type::LLong(_) => types::Type::i64,
            Type::Float => panic!(),
            Type::Double => types::Type::f64,
            Type::Pointer(id) => {
                let inner = compound_types[*id].as_pointer();
                let inner = inner.conv(compound_types, types);
                types.new_pointer_ty(inner)
            }
            Type::Array(id) => {
                let (inner, len) = compound_types[*id].as_array();
                let inner = inner.conv(compound_types, types);
                types.new_array_ty(inner, len as usize)
            }
            Type::Func(id) => {
                let (ret, params, _vararg) = compound_types[*id].as_func();
                let ret = ret.conv(compound_types, types);
                let params = params
                    .into_iter()
                    .map(|p| p.conv(compound_types, types))
                    .collect();
                types.new_function_ty(ret, params)
            }
            Type::Struct(id) => {
                let (_name, fields) = compound_types[*id].as_struct();
                let fields = fields
                    .iter()
                    .map(|t| t.conv(compound_types, types))
                    .collect();
                types.new_struct_ty(fields)
            }
            Type::Union(_id) => panic!(),
        }
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

impl Index<Type> for CompoundTypes {
    type Output = CompoundType;

    fn index(&self, ty: Type) -> &Self::Output {
        match ty {
            Type::Pointer(id) | Type::Array(id) | Type::Func(id) => &self.0[id],
            _ => panic!(),
        }
    }
}

impl IndexMut<Type> for CompoundTypes {
    fn index_mut(&mut self, ty: Type) -> &mut Self::Output {
        match ty {
            Type::Pointer(id) | Type::Array(id) | Type::Func(id) => &mut self.0[id],
            _ => panic!(),
        }
    }
}
