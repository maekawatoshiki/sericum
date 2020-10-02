use super::ast::AST;
use cilk::ir::{builder::Builder, module::Module};

pub struct Codegenerator {}

pub struct FunctionCodeGenerator {}

impl Codegenerator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn generate(&mut self, nodes: &Vec<AST>) {}
}
