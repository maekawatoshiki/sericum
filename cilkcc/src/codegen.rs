use super::{
    ast,
    ast::AST,
    token::SourceLoc,
    types::{CompoundTypes, StorageClass, Type, TypeConversion},
};
use cilk::ir::{
    builder::{IRBuilder, IRBuilderWithFunction, IRBuilderWithModuleAndFuncId},
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
    builder: IRBuilderWithModuleAndFuncId<'a>,
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
        param_names: &Vec<String>,
        name: &String,
        body: &AST,
    ) -> Result<Value> {
        FunctionCodeGenerator::new(
            &mut self.module,
            self.compound_types,
            &mut self.variables,
            ty,
            param_names,
            name,
            body,
        )
    }
}

impl<'a> FunctionCodeGenerator<'a> {
    pub fn new(
        module: &'a mut Module,
        compound_types: &'a mut CompoundTypes,
        variables: &'a mut Variables,
        ty: &Type,
        param_names: &Vec<String>,
        name: &String,
        body: &AST,
    ) -> Result<Value> {
        let func_ty = ty.conv(compound_types, &module.types);
        let cilk_func_ty = module.types.compound_ty(func_ty).as_function().clone();
        let func_id = module.create_function(
            name.as_str(),
            cilk_func_ty.ret_ty,
            cilk_func_ty.params_ty.clone(),
        );
        let val = Value::Function(value::FunctionValue {
            func_id,
            ty: func_ty,
        });

        variables.add_local_var(name.clone(), Variable::new(*ty, func_ty, val));
        variables.push_env();

        let mut gen = Self {
            builder: {
                let mut builder = IRBuilderWithModuleAndFuncId::new(module, func_id);
                let entry = builder.append_basic_block();
                builder.set_insert_point(entry);
                builder
            },
            compound_types,
            variables,
        };

        for (i, name) in param_names.iter().enumerate() {
            let cilk_ty = cilk_func_ty.params_ty[i];
            let ty = gen.compound_types[*ty].as_func().1[i];
            let val = gen.builder.func_ref().get_param_value(i).unwrap();
            let var = gen.builder.build_alloca(cilk_ty);
            gen.builder.build_store(val, var);
            gen.variables
                .add_local_var(name.clone(), Variable::new(ty, cilk_ty, var));
        }

        gen.generate(body)?;

        gen.variables.pop_env();

        Ok(val)
    }

    pub fn generate(&mut self, body: &AST) -> Result<Value> {
        match &body.kind {
            ast::Kind::Block(stmts) => self.generate_block(&stmts),
            ast::Kind::If { cond, then_, else_ } => self.generate_if(cond, then_, else_),
            ast::Kind::While { cond, body } => self.generate_while(cond, body),
            ast::Kind::Int { n, bits: 32 } => Ok(Value::new_imm_int32(*n as i32)),
            ast::Kind::VariableDecl(ty, name, sclass, val) => {
                self.generate_var_decl(*ty, name, *sclass, val.as_ref().map(|v| &**v))
            }
            ast::Kind::Variable(_, name) => self
                .variables
                .find_var(name.as_str())
                .ok_or_else(|| Error::Message(body.loc, "variable not found".to_string()))
                .map(|ok| ok.val),
            ast::Kind::Assign { dst, src } => self.generate_assign(dst, src),
            ast::Kind::Load(val) => self.generate_load(val),
            ast::Kind::BinaryOp(op, lhs, rhs) => self.generate_binary_op(*op, lhs, rhs),
            ast::Kind::TernaryOp(cond, then_, else_) => {
                self.generate_ternary_op(cond, then_, else_)
            }
            ast::Kind::FuncCall(f, args) => self.generate_func_call(f, args),
            ast::Kind::Return(val) => self.generate_return(val.as_ref().map(|v| &**v)),
            _ => panic!(),
        }
    }

    fn generate_block(&mut self, stmts: &Vec<AST>) -> Result<Value> {
        for stmt in stmts {
            self.generate(stmt)?;
        }
        Ok(Value::None)
    }

    fn generate_if(&mut self, cond: &AST, then_: &AST, else_: &AST) -> Result<Value> {
        let cond = self.generate(cond)?;

        let then_block = self.builder.append_basic_block();
        let else_block = self.builder.append_basic_block();
        let merge_block = self.builder.append_basic_block();

        self.builder.build_cond_br(cond, then_block, else_block);

        self.builder.set_insert_point(then_block);

        self.generate(then_)?;

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(merge_block);
        }

        self.builder.set_insert_point(else_block);

        self.generate(else_)?;

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(merge_block);
        }

        self.builder.set_insert_point(merge_block);

        Ok(Value::None)
    }

    fn generate_while(&mut self, cond: &AST, body: &AST) -> Result<Value> {
        let header_block = self.builder.append_basic_block();
        let body_block = self.builder.append_basic_block();
        let post_block = self.builder.append_basic_block();

        self.builder.build_br(header_block);
        self.builder.set_insert_point(header_block);

        let cond = self.generate(cond)?;
        self.builder.build_cond_br(cond, body_block, post_block);

        self.builder.set_insert_point(body_block);

        self.generate(body)?;

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(header_block);
        }

        self.builder.set_insert_point(post_block);

        Ok(Value::None)
    }

    fn generate_var_decl(
        &mut self,
        ty: Type,
        name: &String,
        _sclass: StorageClass,
        _val: Option<&AST>,
    ) -> Result<Value> {
        let cilk_ty = ty.conv(self.compound_types, &self.builder.module().unwrap().types);
        let mut builder = IRBuilderWithFunction::new(self.builder.func_ref_mut());
        let entry = builder.func_ref().get_entry_block().unwrap();
        builder.set_insert_point_at(0, entry);
        let alloca = builder.build_alloca(cilk_ty);
        self.variables
            .add_local_var(name.clone(), Variable::new(ty, cilk_ty, alloca));

        // Adjust the insert point of global builder
        let bb = self.builder.block();
        let pt = self.builder.insert_point();
        if bb == Some(entry) {
            self.builder.set_insert_point_at(pt + 1, entry);
        }

        Ok(Value::None)
    }

    fn generate_assign(&mut self, dst: &AST, src: &AST) -> Result<Value> {
        let dst = self.generate(retrieve_from_load(dst))?;
        let src = self.generate(src)?;
        self.builder.build_store(src, dst);
        Ok(self.builder.build_load(dst))
    }

    fn generate_load(&mut self, val: &AST) -> Result<Value> {
        let val = self.generate(val)?;
        if let types::Type::Pointer(id) = val.get_type() {
            let inner = *self
                .builder
                .module()
                .unwrap()
                .types
                .compound_ty(id)
                .as_pointer();
            match inner {
                types::Type::Array(_) => Ok(val),
                _ => Ok(self.builder.build_load(val)),
            }
        } else {
            panic!()
        }
    }

    fn generate_binary_op(&mut self, op: ast::BinaryOp, lhs: &AST, rhs: &AST) -> Result<Value> {
        let lhs = self.generate(lhs)?;
        let rhs = self.generate(rhs)?;
        match op {
            ast::BinaryOp::Eq => Ok(self.builder.build_icmp(ICmpKind::Eq, lhs, rhs)),
            ast::BinaryOp::Le => Ok(self.builder.build_icmp(ICmpKind::Le, lhs, rhs)),
            ast::BinaryOp::Add => {
                if let types::Type::Pointer(_) = lhs.get_type() {
                    return self.generate_ptr_binary_op(op, lhs, rhs);
                }
                // TODO: ???
                if let types::Type::Pointer(_) = rhs.get_type() {
                    return self.generate_ptr_binary_op(op, rhs, lhs);
                }

                Ok(self.builder.build_add(lhs, rhs))
            }
            ast::BinaryOp::Sub => Ok(self.builder.build_sub(lhs, rhs)),
            _ => unimplemented!(),
        }
    }

    fn generate_ptr_binary_op(
        &mut self,
        op: ast::BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value> {
        let is_elem_array = match lhs.get_type() {
            types::Type::Pointer(id) => matches!(
                self.builder
                    .module()
                    .unwrap()
                    .types
                    .compound_ty(id)
                    .as_pointer(),
                types::Type::Array(_)
            ),
            _ => panic!(),
        };
        match op {
            ast::BinaryOp::Add => {
                if is_elem_array {
                    Ok(self
                        .builder
                        .build_gep(lhs, vec![Value::new_imm_int32(0), rhs]))
                } else {
                    Ok(self.builder.build_gep(lhs, vec![rhs]))
                }
            }
            _ => unimplemented!(),
        }
    }

    fn generate_ternary_op(&mut self, cond: &AST, then_: &AST, else_: &AST) -> Result<Value> {
        let cond = self.generate(cond)?;

        let then_block = self.builder.append_basic_block();
        let else_block = self.builder.append_basic_block();
        let merge_block = self.builder.append_basic_block();

        self.builder.build_cond_br(cond, then_block, else_block);

        self.builder.set_insert_point(then_block);

        let then_ = self.generate(then_)?;

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(merge_block);
        }

        self.builder.set_insert_point(else_block);

        let else_ = self.generate(else_)?;

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(merge_block);
        }

        self.builder.set_insert_point(merge_block);

        if matches!(then_, Value::None) || matches!(else_, Value::None) {
            return Ok(Value::None);
        }

        Ok(self
            .builder
            .build_phi(vec![(then_, then_block), (else_, else_block)]))
    }

    fn generate_func_call(&mut self, f: &AST, args: &Vec<AST>) -> Result<Value> {
        let f = retrieve_from_load(f);
        let mut args_ = vec![];
        for arg in args {
            args_.push(self.generate(arg).unwrap());
        }
        match &f.kind {
            ast::Kind::Variable(_, name) => {
                let var = self
                    .variables
                    .find_var(name.as_str())
                    .ok_or_else(|| Error::Message(f.loc, "variable not found".to_string()))?;
                Ok(self.builder.build_call(var.val, args_))
            }
            _ => unimplemented!(),
        }
    }

    fn generate_return(&mut self, val: Option<&AST>) -> Result<Value> {
        let val = if let Some(val) = val {
            self.generate(val)
        } else {
            Ok(Value::None)
        };
        Ok(self.builder.build_ret(val?))
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
