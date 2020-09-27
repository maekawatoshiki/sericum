extern crate rustc_hash;

use std::path::PathBuf;

pub mod lexer;
pub mod token;

pub fn compile(path: PathBuf) {
    let _lexer = lexer::Lexer::new(path);
}
