use super::ast::AST;
use super::lexer::{Error, Lexer, Result};

pub struct Parser<'a> {
    pub lexer: &'a mut Lexer,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Parser<'a> {
        Self { lexer }
    }

    pub fn run(&mut self) -> Result<Vec<AST>> {
        let mut node = vec![];
        while let Ok(n) = self.read_toplevel() {
            node.push(n);
        }
        Ok(node)
    }

    fn read_toplevel(&mut self) -> Result<AST> {
        todo!()
    }
}
