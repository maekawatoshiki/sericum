use crate::ir::types::Type;

/// Machine Value Type
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MVType {
    Invalid,
    Void,
    i1,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,
}

impl From<Type> for MVType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Void => MVType::Void,
            Type::i1 => MVType::i1,
            Type::i8 => MVType::i8,
            Type::i32 => MVType::i32,
            Type::i64 => MVType::i64,
            Type::f64 => MVType::f64,
            Type::Pointer(_) => MVType::i64,
            Type::Array(_) | Type::Struct(_) | Type::Function(_) => MVType::Invalid,
        }
    }
}

impl MVType {
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::i1 | Self::i8 | Self::i16 | Self::i32 | Self::i64
        )
    }

    pub fn size_in_bits(&self) -> i32 {
        match self {
            Self::Invalid => 0,
            Self::Void => 0,
            Self::i1 => 1,
            Self::i8 => 8,
            Self::i16 => 16,
            Self::i32 => 32,
            Self::i64 => 64,
            Self::f32 => 32,
            Self::f64 => 64,
        }
    }

    pub fn size_in_byte(&self) -> i32 {
        match self {
            Self::Invalid => 0,
            Self::Void => 0,
            Self::i1 => 1,
            Self::i8 => 1,
            Self::i16 => 2,
            Self::i32 => 4,
            Self::i64 => 8,
            Self::f32 => 4,
            Self::f64 => 8,
        }
    }
}
