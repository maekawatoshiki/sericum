use super::{function::*, types::*, DumpToString};
use id_arena::*;
use std::fmt;

/// A representation of Module, that has name, some functions and type definisions.
#[derive(Clone)]
pub struct Module {
    /// module name
    pub name: String,
    /// functions that a module has.
    pub functions: Arena<Function>,
    /// type definitions in a module.
    pub types: Types,
}

impl Module {
    /// Creates an empty `Module`.
    ///
    /// # Arguments
    ///
    /// * `name` ... `Module::new()` names the created module it.
    ///
    /// Initialized module is really "empty", so it has no functions yet.
    /// See [create_function()](struct.Module.html#method.create_function) when you want to create some functions in module.
    ///
    /// # Examples
    ///
    /// ```
    /// use cilk::module::Module;
    /// let m = Module::new("sample");
    /// assert_eq!(m.functions.len(), 0);
    /// ```
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            functions: Arena::new(),
            types: Types::new(),
        }
    }

    /// allocate a [Function](../function/struct.Function.html) and insert into the method's receiver(Module).
    /// The owner of the created functions is also receiver,
    /// so the function will be freed when the owner drops.
    ///
    /// # Arguments
    ///
    /// * `name` ... function's name
    /// * `ret_ty` ... a [Type](../types/enum.Type.html) that the function returns.
    /// * `params_ty` ... a sequence of [Type](../types/enum.Type.html) that are passed to function as parameters.
    ///
    /// # Examples
    /// ```
    /// use cilk::{module::Module, types::Type};
    ///
    /// let mut m1 = Module::new("owner");
    /// let m2 = Module::new("other");
    ///
    /// let _f1_id = m1.create_function("f1", Type::Void, vec![]);
    /// ```
    pub fn create_function(
        &mut self,
        name: &str,
        ret_ty: Type,
        params_ty: Vec<Type>,
    ) -> FunctionId {
        Function::new(self, name, ret_ty, params_ty)
    }

    /// Add function to the receiver of this method.
    /// See also [Function](../function/struct.Function.html) how this method is used.
    pub fn add_function(&mut self, f: Function) -> FunctionId {
        let id = self.functions.alloc(f);
        self.function_ref_mut(id).id = Some(id);
        id
    }

    /// get a reference of the function that has the id.
    ///
    /// # Arguments
    ///
    /// * id ... an identifier of [Function](../function/struct.Function.html) what you want to reference.
    pub fn function_ref(&self, id: FunctionId) -> &Function {
        &self.functions[id]
    }

    /// get a mutable reference of the function that has the id.
    ///
    /// # Arguments
    ///
    /// * id ... an identifier of [Function](../function/struct.Function.html) what you want to reference.
    pub fn function_ref_mut(&mut self, id: FunctionId) -> &mut Function {
        &mut self.functions[id]
    }

    /// find a function that is defined in the module by name.
    ///
    /// # Examples
    /// ```
    /// use cilk::{module::Module, types::Type};
    ///
    /// let mut m1 = Module::new("owner");
    /// let m2 = Module::new("other");
    ///
    /// let _f1_id = m1.create_function("f1", Type::Void, vec![]);
    ///
    /// // check which module has the function.
    /// assert!(m1.find_function("f1").is_some());
    /// assert!(m2.find_function("f1").is_none())
    /// ```
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
        for (_, func) in &self.functions {
            writeln!(f, "{}", self.dump(func))?;
        }
        fmt::Result::Ok(())
    }
}
