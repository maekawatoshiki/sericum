extern crate id_arena;
extern crate rustc_hash;

use std::path::PathBuf;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

pub fn compile(path: PathBuf) {
    let mut lexer = lexer::Lexer::new(path);
    while let Ok(tok) = lexer.get_token() {
        println!("{:?}", tok);
    }
}
