extern crate id_arena;
extern crate rustc_hash;

use std::path::PathBuf;

pub mod lexer;
pub mod token;

pub fn compile(path: PathBuf) {
    let mut lexer = lexer::Lexer::new(path);
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
    println!("{:?}", lexer.read_token());
}
