use super::{function::*, types::*};
use crate::util::allocator::*;
use id_arena::*;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub struct ModuleRef(pub Raw<Module>);

#[derive(Clone, Debug)]
pub struct Module {
    pub name: String,
    pub functions: Arena<Function>,
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

impl ModuleRef {
    pub fn create_function(
        &mut self,
        name: &str,
        ret_ty: Type,
        params_ty: Vec<Type>,
    ) -> FunctionId {
        let func = Function::new(*self, name, ret_ty, params_ty);
        self.add_function(func)
    }
}

impl Deref for ModuleRef {
    type Target = Module;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ModuleRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
