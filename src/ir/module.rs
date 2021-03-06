use super::{
    basic_block::BasicBlocks, builder::IRBuilderWithModuleAndFuncId, constant_pool::*, function::*,
    global_val::*, types::*, value,
};
use crate::codegen::is_internal_function;
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

    /// Constant values attached to this module
    pub const_pool: ConstantPool,

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
    /// use sericum::module::Module;
    /// let m = Module::new("sample");
    /// assert_eq!(m.functions.len(), 0);
    /// ```
    pub fn new(name: &str) -> Self {
        let types = Types::new();
        Self {
            name: name.to_string(),
            functions: Arena::new(),
            global_vars: GlobalVariables::new(types.clone()),
            const_pool: ConstantPool::new(types.clone()),
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
    /// use sericum::{module::Module, types::Type};
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

    pub fn create_function_with_type(&mut self, name: &str, func_ty: Type) -> FunctionId {
        self.add_function(Function {
            name: name.to_string(),
            ty: func_ty,
            basic_blocks: BasicBlocks::new(),
            inst_table: Arena::new(),
            id: None,
            analyses: vec![],
            types: self.types.clone(),
            is_internal: is_internal_function(name),
        })
    }

    /// Attaches an existing function to the module.
    /// See also [Function](../function/struct.Function.html) to understand how this method is used.
    pub fn add_function(&mut self, f: Function) -> FunctionId {
        let ptr_ty = self.types.new_pointer_ty(f.ty);
        let id = self.functions.alloc(f);
        self.types
            .base
            .borrow_mut()
            .func_ptr_types
            .insert(id, ptr_ty);
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
    /// use sericum::{module::Module, types::Type};
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

    pub fn create_string(&mut self, s: String) -> value::Value {
        let i8_arr = self.types.new_array_ty(Type::i8, s.len() + 1);
        let id = self.const_pool.add(Constant {
            kind: ConstantKind::String(s),
            ty: i8_arr,
        });
        value::Value::Constant(value::ConstantValue { id })
    }

    pub fn create_constant(&mut self, c: Constant) -> value::Value {
        // let ty = self.types.new_pointer_ty(c.ty);
        let id = self.const_pool.add(c);
        value::Value::Constant(value::ConstantValue { id })
    }

    pub fn ir_builder<'a>(&'a mut self, func_id: FunctionId) -> IRBuilderWithModuleAndFuncId<'a> {
        IRBuilderWithModuleAndFuncId::new(self, func_id)
    }
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Module (name: {})", self.name)?;
        writeln!(f, "{:?}", self.global_vars)?;
        writeln!(f, "{:?}", self.const_pool)?;
        for (_, func) in &self.functions {
            writeln!(f, "{}", func.dump(self))?;
        }
        fmt::Result::Ok(())
    }
}
