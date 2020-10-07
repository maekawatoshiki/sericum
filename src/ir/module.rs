use super::{function::*, global_val::*, types::*, DumpToString};
use id_arena::*;
use std::fmt;

/// A representation of Module.
/// Module has its name, functions and type definisions.
#[derive(Clone)]
pub struct Module {
    /// Module name
    pub name: String,

    /// Functions attached to this module
    pub functions: Arena<Function>,

    /// Global varaibles attached to this module
    pub global_vars: GlobalVariables,

    /// Type definitions in this module
    pub types: Types,
}

impl Module {
    /// Creates an empty `Module` named `name`.
    ///
    /// Use [create_function()](struct.Module.html#method.create_function) to create a function in the module.
    ///
    /// # Examples
    ///
    /// ```
    /// use cilk::module::Module;
    /// let m = Module::new("sample");
    /// assert_eq!(m.functions.len(), 0);
    /// ```
    pub fn new(name: &str) -> Self {
        let types = Types::new();
        Self {
            name: name.to_string(),
            functions: Arena::new(),
            global_vars: GlobalVariables::new(types.clone()),
            types,
        }
    }

    /// Creates a [Function](../function/struct.Function.html) and attaches it to the module.
    /// All the functions attached to the module will be freed when the module drops.
    ///
    /// # Arguments
    ///
    /// * `name` : Function name
    /// * `ret_ty` :  A [Type](../types/enum.Type.html) that the function returns
    /// * `params_ty` : A sequence of [Type](../types/enum.Type.html)s passed to the function as parameters
    ///
    /// # Examples
    /// ```
    /// use cilk::{module::Module, types::Type};
    /// let mut m = Module::new("owner");
    /// let _f_id = m.create_function("f", Type::Void, vec![]);
    /// ```
    pub fn create_function(
        &mut self,
        name: &str,
        ret_ty: Type,
        params_ty: Vec<Type>,
    ) -> FunctionId {
        Function::new(self, name, ret_ty, params_ty)
    }

    /// Attaches an existing function to the module.
    /// See also [Function](../function/struct.Function.html) to understand how this method is used.
    pub fn add_function(&mut self, f: Function) -> FunctionId {
        let id = self.functions.alloc(f);
        self.function_ref_mut(id).id = Some(id);
        id
    }

    /// Gets a reference to the corresponding function with the given id
    ///
    /// # Arguments
    ///
    /// * `id` :  An identifier of [Function](../function/struct.Function.html) you want to get a reference to
    pub fn function_ref(&self, id: FunctionId) -> &Function {
        &self.functions[id]
    }

    /// Gets a mutable reference to the corresponding function with the given id
    ///
    /// # Arguments
    ///
    /// * `id` : An identifier of [Function](../function/struct.Function.html) you want to get a reference to
    pub fn function_ref_mut(&mut self, id: FunctionId) -> &mut Function {
        &mut self.functions[id]
    }

    /// Finds a function defined in the module by name
    ///
    /// # Examples
    /// ```
    /// use cilk::{module::Module, types::Type};
    ///
    /// let mut m1 = Module::new("owner");
    /// let m2 = Module::new("other");
    ///
    /// let _f_id = m1.create_function("f", Type::Void, vec![]);
    ///
    /// // Check which module has the function
    /// assert!(m1.find_function("f").is_some());
    /// assert!(m2.find_function("f").is_none())
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
        writeln!(f, "{:?}", self.global_vars)?;
        for (_, func) in &self.functions {
            writeln!(f, "{}", self.dump(func))?;
        }
        fmt::Result::Ok(())
    }
}
