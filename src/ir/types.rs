#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int32,
    Pointer(Box<Type>),
    Function(Box<FunctionType>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub ret_ty: Type,
    pub params_ty: Vec<Type>,
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
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Int32 => "i32".to_string(),
            Type::Pointer(e) => format!("{}*", e.to_string()),
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
