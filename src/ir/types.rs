#[derive(Debug, Clone)]
pub enum Type {
    Int32,
}

impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Type::Int32 => "i32".to_string(),
        }
    }
}
