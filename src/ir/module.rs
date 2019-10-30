use super::{function::*, types::*};
use crate::util::allocator::*;
use id_arena::*;

#[derive(Clone, Debug)]
pub struct Module {
    pub name: String,
    pub functions: Arena<Function>,
}

pub trait ModuleTrait {
    fn create_function(&self, name: &str, ret_ty: Type, params_ty: Vec<Type>) -> FunctionId;
    fn add_function(&self, f: Function) -> FunctionId;
    fn function_ref(&self, id: FunctionId) -> &Function;
    fn function_ref_mut(&self, id: FunctionId) -> &mut Function;
    fn find_function_by_name(&self, name: &str) -> Option<FunctionId>;
}

impl ModuleTrait for Raw<Module> {
    fn create_function(&self, name: &str, ret_ty: Type, params_ty: Vec<Type>) -> FunctionId {
        let func = Function::new(*self, name, ret_ty, params_ty);
        self.add_function(func)
    }

    fn add_function(&self, f: Function) -> FunctionId {
        self.inner_ref_mut().functions.alloc(f)
    }

    fn function_ref(&self, id: FunctionId) -> &Function {
        &self.functions[id]
    }

    fn function_ref_mut(&self, id: FunctionId) -> &mut Function {
        &mut self.inner_ref_mut().functions[id]
    }

    fn find_function_by_name(&self, name: &str) -> Option<FunctionId> {
        for (id, func) in &self.functions {
            if func.name == name {
                return Some(id);
            }
        }
        None
    }
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

    pub fn find_function_by_name(&self, name: &str) -> Option<FunctionId> {
        for (id, func) in &self.functions {
            if func.name == name {
                return Some(id);
            }
        }
        None
    }
}
