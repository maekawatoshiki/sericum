use super::{function::*, types::*, DumpToString};
use id_arena::*;

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
