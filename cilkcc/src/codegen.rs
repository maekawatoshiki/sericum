use super::{
    ast,
    ast::AST,
    token::SourceLoc,
    types::{CompoundTypes, StorageClass, Type, TypeConversion},
};
use cilk::ir::{
    builder::{Builder, FunctionIdWithModule},
    function::FunctionId,
    module::Module,
    opcode::ICmpKind,
    types, value,
    value::Value,
};
use rustc_hash::FxHashMap;
use std::result;

pub struct Codegenerator<'a> {
    pub module: Module,
    variables: Variables,
    compound_types: &'a mut CompoundTypes,
}

pub struct FunctionCodeGenerator<'a> {
    builder: Builder<FunctionIdWithModule<'a>>,
    compound_types: &'a mut CompoundTypes,
    variables: &'a mut Variables,
}

pub struct Variables(Vec<FxHashMap<String, Variable>>);

pub struct Variable {
    pub ty: Type,
    pub cilk_ty: types::Type,
    pub val: Value,
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Message(SourceLoc, String),
}

impl<'a> Codegenerator<'a> {
    pub fn new(compound_types: &'a mut CompoundTypes) -> Self {
        Self {
            module: Module::new("cilkcc"),
            variables: Variables::new(),
            compound_types,
        }
    }

    pub fn generate(&mut self, node: &AST) -> Result<Value> {
        match &node.kind {
            ast::Kind::Block(stmts) => self.generate_block(&stmts),
            ast::Kind::FuncDef {
                ty,
                param_names,
                name,
                body,
            } => self.generate_func_def(&ty, &param_names, &name, &body),
            _ => unimplemented!(),
        }
    }

    fn generate_block(&mut self, stmts: &Vec<AST>) -> Result<Value> {
        for stmt in stmts {
            self.generate(stmt)?;
        }
        Ok(Value::None)
    }

    fn generate_func_def(
        &mut self,
        ty: &Type,
        _param_names: &Vec<String>,
        name: &String,
        body: &AST,
    ) -> Result<Value> {
        let func_ty_ = ty.conv(self.compound_types, &self.module.types);
        let func_ty = self
            .module
            .types
            .base
            .borrow()
            .as_function_ty(func_ty_)
            .unwrap()
            .clone();
        let func_id = self
            .module
            .create_function(name.as_str(), func_ty.ret_ty, func_ty.params_ty);
        let val = Value::Function({
            value::FunctionValue {
                func_id,
                ty: func_ty_,
            }
        });
        self.variables
            .add_local_var(name.clone(), Variable::new(*ty, func_ty_, val));
        self.variables.push_env();
        let mut gen = FunctionCodeGenerator::new(
            &mut self.module,
            self.compound_types,
            &mut self.variables,
            func_id,
        );
        gen.generate(body);
        self.variables.pop_env();
        Ok(val)
    }
}

impl<'a> FunctionCodeGenerator<'a> {
    pub fn new(
        module: &'a mut Module,
        compound_types: &'a mut CompoundTypes,
        variables: &'a mut Variables,
        func: FunctionId,
    ) -> Self {
        Self {
            builder: {
                let mut builder = Builder::new(FunctionIdWithModule::new(module, func));
                let entry = builder.append_basic_block();
                builder.set_insert_point(entry);
                builder
            },
            compound_types,
            variables,
        }
    }

    pub fn generate(&mut self, body: &AST) -> Option<Value> {
        match &body.kind {
            ast::Kind::Block(stmts) => self.generate_block(&stmts),
            ast::Kind::If { cond, then_, else_ } => self.generate_if(cond, then_, else_),
            ast::Kind::Int { n, bits: 32 } => Some(Value::new_imm_int32(*n as i32)),
            ast::Kind::VariableDecl(ty, name, sclass, val) => {
                self.generate_var_decl(*ty, name, *sclass, val.as_ref().map(|v| &**v))
            }
            ast::Kind::Assign { dst, src } => self.generate_assign(dst, src),
            ast::Kind::Load(val) => self.generate_load(val),
            ast::Kind::BinaryOp(op, lhs, rhs) => self.generate_binary_op(*op, lhs, rhs),
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

    fn generate_if(&mut self, cond: &AST, then_: &AST, else_: &AST) -> Option<Value> {
        let cond = self.generate(cond)?;

        let then_block = self.builder.append_basic_block();
        let else_block = self.builder.append_basic_block();
        let merge_block = self.builder.append_basic_block();

        self.builder.build_cond_br(cond, then_block, else_block);

        self.builder.set_insert_point(then_block);

        self.generate(then_);

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(merge_block);
        }

        self.builder.set_insert_point(else_block);

        self.generate(else_);

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(merge_block);
        }

        self.builder.set_insert_point(merge_block);

        None
    }

    fn generate_var_decl(
        &mut self,
        ty: Type,
        name: &String,
        _sclass: StorageClass,
        _val: Option<&AST>,
    ) -> Option<Value> {
        let cilk_ty = ty.conv(self.compound_types, &self.builder.func.module.types);
        let alloca = self.builder.build_alloca(cilk_ty);
        self.variables
            .add_local_var(name.clone(), Variable::new(ty, cilk_ty, alloca));
        None
    }

    fn generate_assign(&mut self, dst: &AST, src: &AST) -> Option<Value> {
        let dst = retrieve_from_load(dst);
        let src = self.generate(src).unwrap();
        match &dst.kind {
            ast::Kind::Variable(_, name) => {
                let dst = self
                    .variables
                    .find_var(name.as_str())
                    .expect("var not found");
                Some(self.builder.build_store(src, dst.val))
            }
            _ => unimplemented!(),
        }
    }

    fn generate_load(&mut self, val: &AST) -> Option<Value> {
        match &val.kind {
            ast::Kind::Variable(_, name) => {
                let var = self
                    .variables
                    .find_var(name.as_str())
                    .expect("var not found");
                Some(self.builder.build_load(var.val))
            }
            _ => unimplemented!(),
        }
    }

    fn generate_binary_op(&mut self, op: ast::BinaryOp, lhs: &AST, rhs: &AST) -> Option<Value> {
        let lhs = self.generate(lhs)?;
        let rhs = self.generate(rhs)?;
        match op {
            ast::BinaryOp::Eq => Some(self.builder.build_icmp(ICmpKind::Eq, lhs, rhs)),
            _ => unimplemented!(),
        }
    }

    fn generate_return(&mut self, val: Option<&AST>) -> Option<Value> {
        let val = if let Some(val) = val {
            self.generate(val)
        } else {
            Some(Value::None)
        };
        Some(self.builder.build_ret(val.unwrap()))
    }
}

impl Variables {
    pub fn new() -> Self {
        Self(vec![FxHashMap::default()])
    }

    pub fn push_env(&mut self) {
        self.0.push(FxHashMap::default())
    }

    pub fn pop_env(&mut self) {
        self.0.pop();
    }

    fn add_local_var(&mut self, name: String, var: Variable) {
        self.0.last_mut().unwrap().insert(name, var);
    }

    fn find_var(&self, name: &str) -> Option<&Variable> {
        self.0.iter().rev().find_map(|vars| vars.get(name))
    }
}

impl Variable {
    pub fn new(ty: Type, cilk_ty: types::Type, val: Value) -> Self {
        Self { ty, cilk_ty, val }
    }
}

pub fn retrieve_from_load<'a>(ast: &'a AST) -> &'a AST {
    match ast.kind {
        ast::Kind::Load(ref x) | ast::Kind::UnaryOp(ast::UnaryOp::Deref, ref x) => x,
        _ => ast,
    }
}
