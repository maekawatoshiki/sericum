use super::{function::*, global_val::*, types::*, DumpToString};
use id_arena::*;
use std::fmt;

#[derive(Clone)]
pub struct Module {
    pub name: String,
    pub functions: Arena<Function>,
    pub global_vars: GlobalVariables,
    pub types: Types,
}

impl Module {
    pub fn new(name: &str) -> Self {
        let types = Types::new();
        Self {
            name: name.to_string(),
            functions: Arena::new(),
            global_vars: GlobalVariables::new(types.clone()),
            types,
        }
    }

    pub fn create_function(
        &mut self,
        name: &str,
        ret_ty: Type,
        params_ty: Vec<Type>,
    ) -> FunctionId {
        Function::new(self, name, ret_ty, params_ty)
    }

    pub fn add_function(&mut self, f: Function) -> FunctionId {
        let id = self.functions.alloc(f);
        self.function_ref_mut(id).id = Some(id);
        id
    }

    pub fn function_ref(&self, id: FunctionId) -> &Function {
        &self.functions[id]
    }

    pub fn function_ref_mut(&mut self, id: FunctionId) -> &mut Function {
        &mut self.functions[id]
    }

    pub fn find_function<'a, Name: Into<FunctionName<'a>>>(
        &self,
        name: Name,
    ) -> Option<FunctionId> {
        let name = name.into().0;
        self.functions
            .iter()
            .find_map(|(id, f)| if f.name == name { Some(id) } else { None })
    }

    pub fn dump<T: DumpToString>(&self, obj: T) -> String {
        obj.dump(self)
    }
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Module (name: {})", self.name)?;
        writeln!(f, "{:?}", self.global_vars)?;
        for (_, func) in &self.functions {
            writeln!(f, "{}", self.dump(func))?;
        }
        fmt::Result::Ok(())
    }
}
