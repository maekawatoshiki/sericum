use super::function::*;
use crate::ir::types::Types;
use id_arena::*;
use std::fmt;

pub struct DAGModule {
    pub name: String,
    pub functions: Arena<DAGFunction>,
    pub types: Types,
}

impl DAGModule {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            functions: Arena::new(),
            types: Types::new(),
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

impl fmt::Debug for DAGModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DAGModule: {}", self.name)?;

        for (_, func) in &self.functions {
            func.debug(f, &self.types)?;
        }

        fmt::Result::Ok(())
    }
}
