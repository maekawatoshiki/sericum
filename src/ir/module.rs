use super::function::*;

#[derive(Clone, Debug)]
pub struct Module {
    name: String,
    functions: Vec<Function>,
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            functions: vec![],
        }
    }
}
