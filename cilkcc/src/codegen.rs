use super::{
    ast,
    ast::AST,
    token::SourceLoc,
    types::{CompoundType, CompoundTypes, Sign, StorageClass, Type, TypeConversion},
};
use cilk::ir::{
    builder::IRBuilderWithModuleAndFuncId,
    constant_pool::{Constant, ConstantArrayElement, ConstantKind},
    opcode::ICmpKind,
    prelude::*,
    types, value,
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
        let mut module = Module::new("cilk");
        {
            let ptr_i64 = module.types.new_pointer_ty(types::Type::i64);
            module.create_function("memcpy", ptr_i64, vec![ptr_i64, ptr_i64, types::Type::i32]);
        }
        Self {
            module,
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
            ast::Kind::VariableDecl(ty, name, sclass, val) => {
                self.generate_var_decl(*ty, name, *sclass, val.as_ref().map(|v| &**v))
            }
            e => unimplemented!("{:?}", e),
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

    fn generate_var_decl(
        &mut self,
        ty: Type,
        name: &String,
        _sclass: StorageClass,
        _val: Option<&AST>,
    ) -> Result<Value> {
        match ty {
            Type::Func(_) => {
                let cilk_ty = ty.conv(self.compound_types, &self.module.types);
                let cilk_func_ty = self.module.types.compound_ty(cilk_ty).as_function().clone();
                let func_id = self.module.create_function(
                    name.as_str(),
                    cilk_func_ty.ret_ty,
                    cilk_func_ty.params_ty.clone(),
                );
                let val = Value::Function(value::FunctionValue {
                    func_id,
                    ty: cilk_ty,
                });
                let p_cilk_ty = self.module.types.new_pointer_ty(cilk_ty);
                let p_ty = self.compound_types.pointer(ty);
                self.variables
                    .add_global_var(name.clone(), Variable::new(p_ty, p_cilk_ty, val));
            }
            _ => todo!(),
        }
        Ok(Value::None)
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

        let p_cilk_ty = module.types.new_pointer_ty(func_ty);
        let p_ty = compound_types.pointer(*ty);
        variables.add_local_var(name.clone(), Variable::new(p_ty, p_cilk_ty, val));
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
            let p_cilk_ty = gen.builder.module().unwrap().types.new_pointer_ty(cilk_ty);
            let p_ty = gen.compound_types.pointer(ty);
            gen.variables
                .add_local_var(name.clone(), Variable::new(p_ty, p_cilk_ty, var));
        }

        gen.generate(body)?;

        gen.variables.pop_env();

        Ok(val)
    }

    pub fn generate(&mut self, body: &AST) -> Result<(Value, Type)> {
        match &body.kind {
            ast::Kind::Block(stmts) => self.generate_block(&stmts),
            ast::Kind::If { cond, then_, else_ } => self.generate_if(cond, then_, else_),
            ast::Kind::While { cond, body } => self.generate_while(cond, body),
            ast::Kind::For {
                init,
                cond,
                step,
                body,
            } => self.generate_for(init, cond.as_ref().map(|v| &**v), step, body),
            ast::Kind::Int { n, bits: 32 } => {
                Ok((Value::new_imm_int32(*n as i32), Type::Int(Sign::Signed)))
            }
            ast::Kind::String(s) => self.generate_string(s),
            ast::Kind::Char(c) => Ok((
                Value::new_imm_int32(*c as u8 as i32),
                Type::Int(Sign::Signed),
            )),
            ast::Kind::ConstArray(ty, elems) => self.generate_const_array(ty, elems),
            ast::Kind::FieldRef(val, name) => self.generate_field_ref(val, name),
            ast::Kind::VariableDecl(ty, name, sclass, val) => {
                self.generate_var_decl(*ty, name, *sclass, val.as_ref().map(|v| &**v))
            }
            ast::Kind::Variable(_, name) => {
                let &Variable { val, ty, .. } = self
                    .variables
                    .find_var(name.as_str())
                    .ok_or_else(|| Error::Message(body.loc, "variable not found".to_string()))?;
                Ok((val, ty))
            }
            ast::Kind::Assign { dst, src } => self.generate_assign(dst, src),
            ast::Kind::Load(val) => self.generate_load(val),
            ast::Kind::UnaryOp(op, expr) => self.generate_unary_op(*op, expr),
            ast::Kind::BinaryOp(op, lhs, rhs) => self.generate_binary_op(*op, lhs, rhs),
            ast::Kind::TernaryOp(cond, then_, else_) => {
                self.generate_ternary_op(cond, then_, else_)
            }
            ast::Kind::FuncCall(f, args) => self.generate_func_call(f, args),
            ast::Kind::Return(val) => self.generate_return(val.as_ref().map(|v| &**v)),
            e => todo!("{:?}", e),
        }
    }

    fn generate_block(&mut self, stmts: &Vec<AST>) -> Result<(Value, Type)> {
        for stmt in stmts {
            self.generate(stmt)?;
        }
        Ok((Value::None, Type::Void))
    }

    fn generate_if(&mut self, cond: &AST, then_: &AST, else_: &AST) -> Result<(Value, Type)> {
        let cond = self.generate(cond)?.0;

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

        Ok((Value::None, Type::Void))
    }

    fn generate_while(&mut self, cond: &AST, body: &AST) -> Result<(Value, Type)> {
        let header_block = self.builder.append_basic_block();
        let body_block = self.builder.append_basic_block();
        let post_block = self.builder.append_basic_block();

        self.builder.build_br(header_block);
        self.builder.set_insert_point(header_block);

        let cond = self.generate(cond)?.0;
        self.builder.build_cond_br(cond, body_block, post_block);

        self.builder.set_insert_point(body_block);

        self.generate(body)?;

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(header_block);
        }

        self.builder.set_insert_point(post_block);

        Ok((Value::None, Type::Void))
    }

    fn generate_for(
        &mut self,
        init: &AST,
        cond: Option<&AST>,
        step: &AST,
        body: &AST,
    ) -> Result<(Value, Type)> {
        let pre_block = self.builder.append_basic_block();
        let loop_block = self.builder.append_basic_block();
        let step_block = self.builder.append_basic_block();
        let post_block = self.builder.append_basic_block();

        self.generate(init)?;

        self.builder.build_br(pre_block);
        self.builder.set_insert_point(pre_block);

        if let Some(cond) = cond {
            let cond = self.generate(cond)?.0;
            self.builder.build_cond_br(cond, loop_block, post_block);
        } else {
            self.builder.build_br(loop_block);
        }

        self.builder.set_insert_point(loop_block);

        self.generate(body)?;

        self.builder.build_br(step_block);
        self.builder.set_insert_point(step_block);

        self.generate(step)?;
        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(pre_block);
        }

        self.builder.set_insert_point(post_block);

        Ok((Value::None, Type::Void))
    }

    fn generate_string(&mut self, s: &String) -> Result<(Value, Type)> {
        let s = self.builder.module_mut().unwrap().create_string(s.clone());
        let gep = self
            .builder
            .build_gep(s, vec![Value::new_imm_int32(0), Value::new_imm_int32(0)]);
        Ok((gep, self.compound_types.pointer(Type::Char(Sign::Signed))))
    }

    fn generate_const_array(
        &mut self,
        array_ty: &Type,
        elements: &Vec<AST>,
    ) -> Result<(Value, Type)> {
        // TODO: Refactoring

        fn conv(
            compound_types: &mut CompoundTypes,
            module: &mut Module,
            element: &AST,
        ) -> ConstantArrayElement {
            match &element.kind {
                ast::Kind::ConstArray(array_ty, elements) => {
                    let (inner, len) = compound_types[*array_ty].as_array();
                    let mut new_elements = vec![];
                    for element in elements {
                        new_elements.push(conv(compound_types, module, element))
                    }
                    while new_elements.len() < len as usize {
                        assert!(inner == Type::Int(Sign::Signed));
                        new_elements.push(ConstantArrayElement::Immediate(
                            value::ImmediateValue::Int32(0),
                        ))
                    }
                    ConstantArrayElement::Array(new_elements)
                }
                ast::Kind::Int { n, bits: 32 } => {
                    ConstantArrayElement::Immediate(value::ImmediateValue::Int32(*n as i32))
                }
                ast::Kind::String(s) => {
                    let i8_arr = module.types.new_array_ty(types::Type::i8, s.len() + 1);
                    let id = module.const_pool.add(Constant {
                        kind: ConstantKind::String(s.to_string()),
                        ty: i8_arr,
                    });
                    ConstantArrayElement::String(id)
                }
                _ => todo!(),
            }
        }

        let (inner, len) = self.compound_types[*array_ty].as_array();
        let mut new_elements = vec![];
        for element in elements {
            new_elements.push(conv(
                &mut self.compound_types,
                &mut self.builder.module_mut().unwrap(),
                element,
            ));
        }
        while new_elements.len() < len as usize {
            assert!(inner == Type::Int(Sign::Signed)); // TODO
            new_elements.push(ConstantArrayElement::Immediate(
                value::ImmediateValue::Int32(0),
            ))
        }

        let kind = ConstantKind::Array(new_elements);
        let cilk_ty = array_ty.conv(self.compound_types, &self.builder.module().unwrap().types);
        let val = self
            .builder
            .module_mut()
            .unwrap()
            .create_constant(Constant { ty: cilk_ty, kind });

        Ok((val, *array_ty))
    }

    fn generate_field_ref(&mut self, val: &AST, name: &String) -> Result<(Value, Type)> {
        let (val, ty) = self.generate(retrieve_from_load(val))?;
        let ty = self.compound_types[ty].inner_ty();
        let (field_idx, &field_ty) = self.compound_types[ty]
            .as_struct()
            .1
            .iter()
            .enumerate()
            .find_map(|(i, (t, n))| if n == name { Some((i, t)) } else { None })
            .unwrap();
        Ok((
            self.builder.build_gep(
                val,
                vec![
                    Value::new_imm_int32(0),
                    Value::new_imm_int32(field_idx as i32),
                ],
            ),
            self.compound_types.pointer(field_ty),
        ))
    }

    fn generate_var_decl(
        &mut self,
        ty: Type,
        name: &String,
        _sclass: StorageClass,
        val: Option<&AST>,
    ) -> Result<(Value, Type)> {
        let (saved_bb, saved_pt) = (self.builder.block(), self.builder.insert_point());
        let cilk_ty = ty.conv(self.compound_types, &self.builder.module().unwrap().types);
        let entry = self.builder.func_ref().get_entry_block().unwrap();
        let pt = self.builder.func_ref().basic_blocks.arena[entry]
            .iseq_ref()
            .iter()
            .enumerate()
            .find_map(|(i, inst)| {
                if self.builder.func_ref().inst_table[inst.as_instruction().id].opcode
                    == Opcode::Alloca
                {
                    None
                } else {
                    Some(i)
                }
            })
            .unwrap_or(0);
        self.builder.set_insert_point_at(pt, entry);
        let alloca = self.builder.build_alloca(cilk_ty);
        let p_cilk_ty = self.builder.module().unwrap().types.new_pointer_ty(cilk_ty);
        let p_ty = self.compound_types.pointer(ty);
        self.variables
            .add_local_var(name.clone(), Variable::new(p_ty, p_cilk_ty, alloca));

        // Adjust the insert point of global builder
        *self.builder.block_mut() = saved_bb;
        *self.builder.insert_point_mut() = saved_pt;
        self.builder.set_insert_point_at_end_of_current_block();

        if let Some(val) = val {
            if let ast::Kind::ConstArray(_, _) = val.kind {
                let (val, ty) = self.generate(val)?;
                let ty = ty.conv(&self.compound_types, &self.builder.module().unwrap().types);
                let gep = self
                    .builder
                    .build_gep(val, vec![Value::new_imm_int32(0), Value::new_imm_int32(0)]);
                let func_id = self
                    .builder
                    .module()
                    .unwrap()
                    .find_function("memcpy")
                    .unwrap();
                let memcpy = Value::Function(value::FunctionValue {
                    func_id,
                    ty: self.builder.module().unwrap().functions[func_id].ty,
                });
                use cilk::ir::types::TypeSize;
                self.builder.build_call(
                    memcpy,
                    vec![
                        alloca,
                        gep,
                        Value::new_imm_int32(
                            ty.size_in_byte(&self.builder.module().unwrap().types) as i32,
                        ),
                    ],
                );
            } else {
                let val = self.generate(val)?.0;
                self.builder.build_store(val, alloca);
            }
        }

        Ok((Value::None, Type::Void))
    }

    fn generate_assign(&mut self, dst: &AST, src: &AST) -> Result<(Value, Type)> {
        let (dst, _) = self.generate(retrieve_from_load(dst))?;
        let (src, ty) = self.generate(src)?;
        self.builder.build_store(src, dst);
        Ok((self.builder.build_load(dst), ty))
    }

    fn generate_load(&mut self, val: &AST) -> Result<(Value, Type)> {
        let (val, ty) = self.generate(val)?;
        if let Type::Pointer(id) = ty {
            let inner = self.compound_types[id].as_pointer();
            if !inner.is_compound() {
                return Ok((
                    self.builder.build_load(val),
                    self.compound_types[ty].inner_ty(),
                ));
            }
            match self.compound_types[inner] {
                CompoundType::Array { inner, .. } => Ok((
                    self.builder
                        .build_gep(val, vec![Value::new_imm_int32(0), Value::new_imm_int32(0)]),
                    self.compound_types.pointer(inner),
                )),
                _ => Ok((
                    self.builder.build_load(val),
                    self.compound_types[ty].inner_ty(),
                )),
            }
        } else {
            panic!()
        }
    }

    fn generate_unary_op(&mut self, op: ast::UnaryOp, expr: &AST) -> Result<(Value, Type)> {
        match op {
            ast::UnaryOp::Addr => self.generate(retrieve_from_load(expr)),
            ast::UnaryOp::Deref => self.generate_load(expr),
            _ => todo!(),
        }
    }

    fn generate_binary_op(
        &mut self,
        op: ast::BinaryOp,
        lhs: &AST,
        rhs: &AST,
    ) -> Result<(Value, Type)> {
        let (lhs, lty) = self.generate(lhs)?;
        let (rhs, rty) = self.generate(rhs)?;

        if let Type::Pointer(_) = lty {
            return self.generate_ptr_binary_op(lty, op, lhs, rhs);
        }

        if let Type::Pointer(_) = rty {
            return self.generate_ptr_binary_op(rty, op, lhs, rhs);
        }

        let conv_ty = if lty.priority() < rty.priority() {
            rty
        } else {
            lty
        };

        if matches!(conv_ty, Type::Float | Type::Double) {
            todo!()
        }

        if conv_ty.is_int() {
            return self.generate_int_binary_op(conv_ty, op, lhs, rhs);
        }

        panic!("{:?}", conv_ty)
    }

    fn generate_int_binary_op(
        &mut self,
        ty: Type,
        op: ast::BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<(Value, Type)> {
        match op {
            ast::BinaryOp::Eq => Ok((
                self.builder.build_icmp(ICmpKind::Eq, lhs, rhs),
                Type::Int(Sign::Signed),
            )),
            ast::BinaryOp::Ne => Ok((
                self.builder.build_icmp(ICmpKind::Ne, lhs, rhs),
                Type::Int(Sign::Signed),
            )),
            ast::BinaryOp::Le => Ok((
                self.builder.build_icmp(ICmpKind::Le, lhs, rhs),
                Type::Int(Sign::Signed),
            )),
            ast::BinaryOp::Lt => Ok((
                self.builder.build_icmp(ICmpKind::Lt, lhs, rhs),
                Type::Int(Sign::Signed),
            )),
            ast::BinaryOp::Gt => Ok((
                self.builder.build_icmp(ICmpKind::Gt, lhs, rhs),
                Type::Int(Sign::Signed),
            )),
            ast::BinaryOp::Ge => Ok((
                self.builder.build_icmp(ICmpKind::Ge, lhs, rhs),
                Type::Int(Sign::Signed),
            )),
            ast::BinaryOp::Add => Ok((self.builder.build_add(lhs, rhs), ty)),
            ast::BinaryOp::Sub => Ok((self.builder.build_sub(lhs, rhs), ty)),
            ast::BinaryOp::Mul => Ok((self.builder.build_mul(lhs, rhs), ty)),
            ast::BinaryOp::Div => Ok((self.builder.build_div(lhs, rhs), ty)),
            ast::BinaryOp::Rem => Ok((self.builder.build_rem(lhs, rhs), ty)),
            _ => unimplemented!(),
        }
    }

    fn generate_ptr_binary_op(
        &mut self,
        ty: Type,
        op: ast::BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<(Value, Type)> {
        match op {
            ast::BinaryOp::Add => Ok((self.builder.build_gep(lhs, vec![rhs]), ty)),
            e => unimplemented!("{:?}", e),
        }
    }

    fn generate_ternary_op(
        &mut self,
        cond: &AST,
        then_: &AST,
        else_: &AST,
    ) -> Result<(Value, Type)> {
        let (cond, _) = self.generate(cond)?;

        let then_block = self.builder.append_basic_block();
        let else_block = self.builder.append_basic_block();
        let merge_block = self.builder.append_basic_block();

        self.builder.build_cond_br(cond, then_block, else_block);

        self.builder.set_insert_point(then_block);

        let (then_, ty) = self.generate(then_)?;

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(merge_block);
        }

        self.builder.set_insert_point(else_block);

        let (else_, _) = self.generate(else_)?;

        if !self.builder.is_last_inst_terminator() {
            self.builder.build_br(merge_block);
        }

        self.builder.set_insert_point(merge_block);

        if matches!(then_, Value::None) || matches!(else_, Value::None) {
            return Ok((Value::None, Type::Void));
        }

        Ok((
            self.builder
                .build_phi(vec![(then_, then_block), (else_, else_block)]),
            ty,
        ))
    }

    fn generate_func_call(&mut self, f: &AST, args: &Vec<AST>) -> Result<(Value, Type)> {
        let f = retrieve_from_load(f);
        let mut args_ = vec![];
        for arg in args {
            args_.push(self.generate(arg).unwrap().0);
        }
        match &f.kind {
            ast::Kind::Variable(_, name) => {
                let var = self
                    .variables
                    .find_var(name.as_str())
                    .ok_or_else(|| Error::Message(f.loc, "variable not found".to_string()))?;
                let i = self.compound_types[var.ty].as_pointer();
                let ret = self.compound_types[i].as_func().0;
                Ok((self.builder.build_call(var.val, args_), ret))
            }
            _ => unimplemented!(),
        }
    }

    fn generate_return(&mut self, val: Option<&AST>) -> Result<(Value, Type)> {
        let val = if let Some(val) = val {
            self.generate(val)?.0
        } else {
            Value::None
        };
        Ok((self.builder.build_ret(val), Type::Void))
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

    fn add_global_var(&mut self, name: String, var: Variable) {
        self.0[0].insert(name, var);
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
