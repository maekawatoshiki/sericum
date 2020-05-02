use crate::parser;
use cilk;

pub struct CodeGenerator {
    module: cilk::module::Module,
}

pub struct CodeGeneratorForFunction<'a> {
    builder: cilk::builder::Builder<'a>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            module: cilk::module::Module::new("minilang"),
        }
    }

    pub fn run(&mut self, input: &str) {
        let module = parser::parser::module(input).expect("parse failed");
        self.run_on_module(module);
    }

    pub fn run_on_module(&mut self, module: parser::Module) {
        println!("{:?}", module);

        // Create prototypes
        let mut worklist = vec![];
        for func in &module.functions {
            worklist.push(
                self.module.create_function(
                    func.name.as_str(),
                    str2type(func.ret_ty.as_str()),
                    func.params
                        .iter()
                        .map(|(_name, ty)| str2type(ty.as_str()))
                        .collect(),
                ),
            );
        }

        // Actaully generate function
        for func_id in worklist {
            CodeGeneratorForFunction::new(&mut self.module, func_id).run()
        }
    }
}

impl<'a> CodeGeneratorForFunction<'a> {
    pub fn new(module: &'a mut cilk::module::Module, func_id: cilk::function::FunctionId) -> Self {
        Self {
            builder: cilk::builder::Builder::new(module, func_id),
        }
    }

    pub fn run(&mut self) {}
}

fn str2type(s: &str) -> cilk::ir::types::Type {
    match s {
        "i32" => cilk::ir::types::Type::Int32,
        "i64" => cilk::ir::types::Type::Int64,
        _ => unimplemented!(),
    }
}
