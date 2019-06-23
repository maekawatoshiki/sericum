#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Int32,
}

impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Int32 => "i32".to_string(),
        }
    }
}
