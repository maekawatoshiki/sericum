use super::value::Value;
use std::fmt;

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Int1,
    Int32,
    Int64,
    F64,
    // TODO: Box -> Rc<RefCell<_>> (for circular structure)
    Pointer(Box<Type>),
    Array(Box<ArrayType>),
    Function(Box<FunctionType>),
    Struct(Box<StructType>),
}

pub trait TypeSize {
    fn size_in_byte(&self) -> usize;
    fn size_in_bits(&self) -> usize;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub ret_ty: Type,
    pub params_ty: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub elem_ty: Type,
    pub len: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub fields_ty: Vec<Type>,
}

impl Type {
    pub fn func_ty(ret_ty: Type, params_ty: Vec<Type>) -> Self {
        Type::Function(Box::new(FunctionType::new(ret_ty, params_ty)))
    }

    pub fn get_function_ty(&self) -> Option<&FunctionType> {
        match self {
            Type::Function(f) => Some(&*f),
            _ => None,
        }
    }

    pub fn get_pointer_ty(&self) -> Type {
        Type::Pointer(Box::new(self.clone()))
    }

    pub fn get_element_ty(&self, index: Option<&Value>) -> Option<&Type> {
        match self {
            Type::Pointer(e) => Some(&**e),
            Type::Array(a) => Some(&a.elem_ty),
            Type::Struct(s) => Some(&s.fields_ty[index.unwrap().as_imm().as_int32() as usize]),
            Type::Void | Type::Int1 | Type::Int32 | Type::Int64 | Type::F64 | Type::Function(_) => {
                Some(self)
            }
        }
    }

    pub fn get_element_ty_with_indices(&self, indices: &[Value]) -> Option<&Type> {
        if indices.len() == 0 {
            return Some(self);
        }

        match self {
            Type::Void | Type::Int1 | Type::Int32 | Type::Int64 | Type::F64 | Type::Function(_) => {
                Some(self)
            }
            Type::Pointer(p) => match indices.len() {
                1 => Some(&**p),
                _ => p.get_element_ty_with_indices(&indices[1..]),
            },
            Type::Array(a) => match indices.len() {
                1 => Some(&a.elem_ty),
                _ => a.elem_ty.get_element_ty_with_indices(&indices[1..]),
            },
            Type::Struct(s) => match indices.len() {
                1 => Some(&s.fields_ty[indices[0].as_imm().as_int32() as usize]),
                _ => s.fields_ty[indices[0].as_imm().as_int32() as usize]
                    .get_element_ty_with_indices(&indices[1..]),
            },
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Int1 => "i1".to_string(),
            Type::Int32 => "i32".to_string(),
            Type::Int64 => "i64".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Pointer(e) => format!("{}*", e.to_string()),
            Type::Array(a) => a.to_string(),
            Type::Function(f) => f.to_string(),
            Type::Struct(s) => s.to_string(),
        }
    }
}

impl FunctionType {
    pub fn new(ret_ty: Type, params_ty: Vec<Type>) -> Self {
        Self { ret_ty, params_ty }
    }

    pub fn to_string(&self) -> String {
        format!(
            "{} ({})",
            self.ret_ty.to_string(),
            self.params_ty.iter().fold("".to_string(), |mut s, p| {
                s += &(p.to_string() + ", ");
                s
            }),
        )
    }
}

impl ArrayType {
    pub fn new(elem_ty: Type, len: usize) -> Self {
        Self { elem_ty, len }
    }

    pub fn to_string(&self) -> String {
        format!("[{} x {}]", self.len, self.elem_ty.to_string(),)
    }
}

impl StructType {
    pub fn new(fields_ty: Vec<Type>) -> Self {
        Self { fields_ty }
    }

    pub fn to_string(&self) -> String {
        format!(
            "struct {{{}}}",
            self.fields_ty
                .iter()
                .fold("".to_string(), |mut s, t| {
                    s += &(t.to_string() + ", ");
                    s
                })
                .trim_matches(&[',', ' '][0..])
        )
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
