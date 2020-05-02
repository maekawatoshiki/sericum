use crate::parser::{self, Node};
use cilk;
use std::collections::HashMap;

pub struct CodeGenerator {
    module: cilk::module::Module,
}

pub struct CodeGeneratorForFunction<'a> {
    builder: cilk::builder::Builder<'a>,
    func: &'a parser::Function,
    vars: Vec<HashMap<String, cilk::value::Value>>,
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
        // println!("{:?}", module);

        // Create prototypes
        let mut worklist = vec![];
        for func in &module.functions {
            worklist.push((
                self.module.create_function(
                    func.name.as_str(),
                    str2type(func.ret_ty.as_str()),
                    func.params
                        .iter()
                        .map(|(_name, ty)| str2type(ty.as_str()))
                        .collect(),
                ),
                func,
            ));
        }

        // Actaully generate function
        for (func_id, func) in worklist {
            CodeGeneratorForFunction::new(&mut self.module, func_id, func).run()
        }

        println!("dump:\n{:?}", self.module);
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
            self.create_var(name.clone(), param);
        }

        let entry = self.builder.append_basic_block();
        self.builder.set_insert_point(entry);

        for stmt in &self.func.body {
            self.run_on_node(stmt);
        }
    }

    pub fn run_on_node(&mut self, node: &Node) -> cilk::value::Value {
        match node {
            Node::Return(e) => {
                let e = self.run_on_node(e);
                self.builder.build_ret(e)
            }
            Node::Add(lhs, rhs) => {
                let lhs = self.run_on_node(lhs);
                let rhs = self.run_on_node(rhs);
                self.builder.build_add(lhs, rhs)
            }
            Node::Identifier(name) => *self.lookup_var(name.as_str()).expect("variable not found"),
            Node::Number(i) => cilk::value::Value::new_imm_int32(*i),
            _ => unimplemented!(),
        }
    }

    pub fn create_var(&mut self, name: String, value: cilk::value::Value) {
        self.vars.last_mut().unwrap().insert(name, value);
    }

    pub fn lookup_var(&self, name: &str) -> Option<&cilk::value::Value> {
        self.vars.last().unwrap().get(name)
    }
}

fn str2type(s: &str) -> cilk::ir::types::Type {
    match s {
        "i32" => cilk::ir::types::Type::Int32,
        "i64" => cilk::ir::types::Type::Int64,
        _ => unimplemented!(),
    }
}
