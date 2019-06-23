use super::function::*;
use id_arena::*;

#[derive(Clone, Debug)]
pub struct Module {
    name: String,
    functions: Arena<Function>,
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            functions: Arena::new(),
        }
    }

    pub fn add_function(&mut self, f: Function) -> FunctionId {
        self.functions.alloc(f)
    }

    pub fn function_ref(&self, id: FunctionId) -> &Function {
        &self.functions[id]
    }

    pub fn function_ref_mut(&mut self, id: FunctionId) -> &mut Function {
        &mut self.functions[id]
    }
}
