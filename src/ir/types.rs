#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int32,
    Pointer(Box<Type>),
}

impl Type {
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
        }
    }
}
