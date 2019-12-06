use super::function::*;
use id_arena::*;

pub struct DAGModule {
    pub name: String,
    pub functions: Arena<DAGFunction>,
}

impl DAGModule {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            functions: Arena::new(),
        }
    }

    pub fn add_function(&mut self, f: DAGFunction) -> DAGFunctionId {
        self.functions.alloc(f)
    }

    pub fn function_ref(&self, id: DAGFunctionId) -> &DAGFunction {
        &self.functions[id]
    }

    pub fn function_ref_mut(&mut self, id: DAGFunctionId) -> &mut DAGFunction {
        &mut self.functions[id]
    }
}
