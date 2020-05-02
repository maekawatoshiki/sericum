use crate::parser;
use cilk;

pub struct CodeGenerator {
    module: cilk::module::Module,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            module: cilk::module::Module::new("minilang"),
        }
    }

    pub fn generate(&mut self, input: &str) {
        let module = parser::parser::module(input).expect("parse failed");
        self.generate_module(module);
    }

    pub fn generate_module(&mut self, module: parser::Module) {
        println!("{:?}", module);
        for func in module.functions {}
    }
}
