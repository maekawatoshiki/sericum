use crate::codegen::common::dag::function::*;
use crate::ir::{constant_pool::ConstantPool, global_val::GlobalVariables, types::Types};
use id_arena::*;
use std::fmt;

pub struct DAGModule {
    pub name: String,
    pub functions: Arena<DAGFunction>,
    pub types: Types,
    pub global_vars: GlobalVariables,
    pub const_pool: ConstantPool,
}

impl DAGModule {
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
            func.debug(f)?;
        }

        fmt::Result::Ok(())
    }
}
