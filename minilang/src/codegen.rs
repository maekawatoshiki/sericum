use crate::parser::{self, Node};
use cilk;
use std::collections::HashMap;

pub struct CodeGenerator {
    pub module: cilk::module::Module,
}

pub struct CodeGeneratorForFunction<'a> {
    builder: cilk::builder::Builder<'a>,
    func: &'a parser::Function,
    vars: Vec<HashMap<String, (bool, cilk::value::Value)>>, // name, (is_arg, val)
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            module: cilk::module::Module::new("minilang"),
        }
    }

    pub fn run(&mut self, input: &str) {
        println!("input:\n{}", input);
        let module = parser::parser::module(input).expect("parse failed");
        self.run_on_module(module);
    }

    pub fn run_on_module(&mut self, module: parser::Module) {
        println!("Parsed: {:?}", module);

        // Create prototypes
        let mut worklist = vec![];
        for func in &module.functions {
            let ret_ty = func.ret_ty.into_cilk_type(&mut self.module.types);
            let params_ty = func
                .params
                .iter()
                .map(|(_name, ty)| ty.into_cilk_type(&mut self.module.types))
                .collect::<Vec<cilk::types::Type>>();
            worklist.push((
                self.module
                    .create_function(func.name.as_str(), ret_ty, params_ty),
                func,
            ));
        }

        // Actaully generate function
        for (func_id, func) in worklist {
            CodeGeneratorForFunction::new(&mut self.module, func_id, func).run()
        }
    }
}

impl<'a> CodeGeneratorForFunction<'a> {
    pub fn new(
        module: &'a mut cilk::module::Module,
        func_id: cilk::function::FunctionId,
        func: &'a parser::Function,
    ) -> Self {
        Self {
            builder: cilk::builder::Builder::new(module, func_id),
            func,
            vars: vec![HashMap::new()],
        }
    }

    pub fn run(&mut self) {
        for (i, (name, _)) in self.func.params.iter().enumerate() {
            let param = self.builder.get_param(i).unwrap();
            self.create_var(name.clone(), true, param);
        }

        let entry = self.builder.append_basic_block();
        self.builder.set_insert_point(entry);

        for stmt in &self.func.body {
            self.run_on_node(stmt);
        }
    }

    pub fn run_on_node(&mut self, node: &Node) -> cilk::value::Value {
        match node {
            Node::VarDecl(name, ty) => {
                let ty = ty.into_cilk_type(&mut self.builder.module.types);
                let alloca = self.builder.build_alloca(ty);
                self.create_var(name.clone(), false, alloca);
                alloca
            }
            Node::Assign(dst, src) => {
                let (_, dst) = *self.lookup_var(dst).expect("variable not found");
                let src = self.run_on_node(src);
                self.builder.build_store(src, dst)
            }
            Node::AssignIndex(base, indices, src) => {
                let (_, base) = *self.lookup_var(base).expect("variable not found");
                let mut indices: Vec<cilk::value::Value> =
                    indices.iter().map(|i| self.run_on_node(i)).collect();
                indices.insert(0, cilk::value::Value::new_imm_int32(0));
                let gep = self.builder.build_gep(base, indices);
                let src = self.run_on_node(src);
                self.builder.build_store(src, gep)
            }
            Node::IfElse(cond, then_, else_) => {
                let cond = self.run_on_node(cond);
                let then_bb = self.builder.append_basic_block();
                let else_bb = self.builder.append_basic_block();
                let merge_bb = if else_.is_some() {
                    self.builder.append_basic_block()
                } else {
                    else_bb
                };
                self.builder.build_cond_br(cond, then_bb, else_bb);
                self.builder.set_insert_point(then_bb);
                for node in then_ {
                    self.run_on_node(node);
                }
                if !self.builder.is_last_inst_terminator() {
                    self.builder.build_br(merge_bb);
                }
                if let Some(else_) = else_ {
                    self.builder.set_insert_point(else_bb);
                    for node in else_ {
                        self.run_on_node(node);
                    }
                    if !self.builder.is_last_inst_terminator() {
                        self.builder.build_br(merge_bb);
                    }
                }
                self.builder.set_insert_point(merge_bb);
                cilk::value::Value::None
            }
            Node::WhileLoop(cond, body) => {
                let header_bb = self.builder.append_basic_block();
                let body_bb = self.builder.append_basic_block();
                let post_bb = self.builder.append_basic_block();
                self.builder.build_br(header_bb);
                self.builder.set_insert_point(header_bb);
                let cond = self.run_on_node(cond);
                self.builder.build_cond_br(cond, body_bb, post_bb);
                self.builder.set_insert_point(body_bb);
                for node in body {
                    self.run_on_node(node);
                }
                if !self.builder.is_last_inst_terminator() {
                    self.builder.build_br(header_bb);
                }
                self.builder.set_insert_point(post_bb);
                cilk::value::Value::None
            }
            Node::Return(e) => {
                let e = self.run_on_node(e);
                self.builder.build_ret(e)
            }
            Node::Eq(lhs, rhs)
            | Node::Lt(lhs, rhs)
            | Node::Le(lhs, rhs)
            | Node::Add(lhs, rhs)
            | Node::Sub(lhs, rhs)
            | Node::Mul(lhs, rhs) => {
                let lhs = self.run_on_node(lhs);
                let rhs = self.run_on_node(rhs);
                match node {
                    Node::Eq(_, _) => self
                        .builder
                        .build_icmp(cilk::opcode::ICmpKind::Eq, lhs, rhs),
                    Node::Lt(_, _) => self
                        .builder
                        .build_icmp(cilk::opcode::ICmpKind::Lt, lhs, rhs),
                    Node::Le(_, _) => self
                        .builder
                        .build_icmp(cilk::opcode::ICmpKind::Le, lhs, rhs),
                    Node::Add(_, _) => self.builder.build_add(lhs, rhs),
                    Node::Sub(_, _) => self.builder.build_sub(lhs, rhs),
                    Node::Mul(_, _) => self.builder.build_mul(lhs, rhs),
                    _ => unreachable!(),
                }
            }
            Node::Identifier(name) => {
                let (is_arg, v) = *self.lookup_var(name.as_str()).expect("variable not found");
                if is_arg {
                    v
                } else {
                    self.builder.build_load(v)
                }
            }
            Node::Index(base, indices) => {
                let (_, base) = *self.lookup_var(base).expect("variable not found");
                let mut indices: Vec<cilk::value::Value> =
                    indices.iter().map(|i| self.run_on_node(i)).collect();
                indices.insert(0, cilk::value::Value::new_imm_int32(0));
                let gep = self.builder.build_gep(base, indices);
                self.builder.build_load(gep)
            }
            Node::Number(i) => cilk::value::Value::new_imm_int32(*i),
            _ => unimplemented!(),
        }
    }

    pub fn create_var(&mut self, name: String, is_arg: bool, value: cilk::value::Value) {
        self.vars.last_mut().unwrap().insert(name, (is_arg, value));
    }

    pub fn lookup_var(&self, name: &str) -> Option<&(bool, cilk::value::Value)> {
        self.vars.last().unwrap().get(name)
    }
}

impl parser::Type {
    pub fn into_cilk_type(&self, types: &mut cilk::types::Types) -> cilk::types::Type {
        match self {
            parser::Type::Int32 => cilk::types::Type::Int32,
            parser::Type::Int64 => cilk::types::Type::Int64,
            parser::Type::Array(len, inner) => {
                let inner = inner.into_cilk_type(types);
                types.new_array_ty(inner, *len)
            }
        }
    }
}
