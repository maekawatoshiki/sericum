extern crate id_arena;
extern crate rustc_hash;

use std::path::PathBuf;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod types;

pub fn compile(path: PathBuf) {
    let mut lexer = lexer::Lexer::new(path);
    println!("{:?}", parser::Parser::new(&mut lexer).parse())
    // loop {
    //     match lexer.get_token() {
    //         Ok(tok) => println!("{:?}", tok),
    //         Err(lexer::Error::Message(loc, msg)) => println!("error: {:?}: {}", loc, msg),
    //         Err(lexer::Error::EOF) => break,
    //     }
    // }
}
