use std::path::PathBuf;

pub mod lexer;

pub fn compile(path: &PathBuf) {
    let _lexer = lexer::Lexer::new(path);
}
