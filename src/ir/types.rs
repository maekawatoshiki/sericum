use super::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int1,
    Int32,
    Pointer(Box<Type>),
    Array(Box<ArrayType>),
    Function(Box<FunctionType>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub ret_ty: Type,
    pub params_ty: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub elem_ty: Type,
    pub len: usize,
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

    pub fn get_element_ty(&self) -> Option<&Type> {
        match self {
            Type::Pointer(e) => Some(&**e),
            Type::Array(a) => Some(&a.elem_ty),
            _ => None,
        }
    }

    pub fn get_element_ty_with_indices(&self, indices: &[Value]) -> Option<&Type> {
        match self {
            Type::Void | Type::Int1 | Type::Int32 | Type::Function(_) => None,
            Type::Pointer(p) => {
                match indices.len() {
                    0 => Some(self),
                    1 => Some(&**p),
                    _ => p.get_element_ty_with_indices(&indices[1..]),
                }
                // p.get_element_ty_with_indices(
            }
            Type::Array(a) => match indices.len() {
                0 => Some(self),
                1 => Some(&a.elem_ty),
                _ => a.elem_ty.get_element_ty_with_indices(&indices[1..]),
            },
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Int1 => "i1".to_string(),
            Type::Int32 => "i32".to_string(),
            Type::Pointer(e) => format!("{}*", e.to_string()),
            Type::Array(a) => a.to_string(),
            Type::Function(f) => f.to_string(),
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
