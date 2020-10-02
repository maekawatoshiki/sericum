use super::{
    ast,
    ast::AST,
    types::{CompoundTypes, Type, TypeConversion},
};
use cilk::ir::{
    builder::{Builder, FunctionIdWithModule},
    function::FunctionId,
    module::Module,
    value::Value,
};

pub struct Codegenerator<'a> {
    pub module: Module,
    compound_types: &'a mut CompoundTypes,
}

pub struct FunctionCodeGenerator<'a> {
    builder: Builder<FunctionIdWithModule<'a>>,
}

impl<'a> Codegenerator<'a> {
    pub fn new(compound_types: &'a mut CompoundTypes) -> Self {
        Self {
            module: Module::new("cilkcc"),
            compound_types,
        }
    }

    pub fn generate(&mut self, node: &AST) {
        match &node.kind {
            ast::Kind::Block(stmts) => self.generate_block(&stmts),
            ast::Kind::FuncDef {
                ty,
                param_names,
                name,
                body,
            } => self.generate_func_def(&ty, &param_names, &name, &body),
            _ => {}
        }
    }

    fn generate_block(&mut self, stmts: &Vec<AST>) {
        for stmt in stmts {
            self.generate(stmt);
        }
    }

    fn generate_func_def(
        &mut self,
        ty: &Type,
        _param_names: &Vec<String>,
        name: &String,
        body: &AST,
    ) {
        let func_ty = ty.conv(self.compound_types, &self.module.types);
        let func_ty = self
            .module
            .types
            .base
            .borrow()
            .as_function_ty(func_ty)
            .unwrap()
            .clone();
        let func_id = self
            .module
            .create_function(name.as_str(), func_ty.ret_ty, func_ty.params_ty);
        let mut gen = FunctionCodeGenerator::new(&mut self.module, func_id);
        gen.generate(body);
    }
}

impl<'a> FunctionCodeGenerator<'a> {
    pub fn new(module: &'a mut Module, func: FunctionId) -> Self {
        Self {
            builder: {
                let mut builder = Builder::new(FunctionIdWithModule::new(module, func));
                let entry = builder.append_basic_block();
                builder.set_insert_point(entry);
                builder
            },
        }
    }

    pub fn generate(&mut self, body: &AST) -> Option<Value> {
        match &body.kind {
            ast::Kind::Block(stmts) => self.generate_block(&stmts),
            ast::Kind::Int { n, bits: 32 } => Some(Value::new_imm_int32(*n as i32)),
            ast::Kind::Return(val) => self.generate_return(val.as_ref().map(|v| &**v)),
            _ => panic!(),
        }
    }

    fn generate_block(&mut self, stmts: &Vec<AST>) -> Option<Value> {
        for stmt in stmts {
            self.generate(stmt);
        }
        None
    }

    fn generate_return(&mut self, val: Option<&AST>) -> Option<Value> {
        let val = if let Some(val) = val {
            self.generate(val)
        } else {
            panic!()
        };
        Some(self.builder.build_ret(val.unwrap()))
    }
}
