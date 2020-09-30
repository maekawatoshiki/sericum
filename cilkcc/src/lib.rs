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
    match parser::Parser::new(&mut lexer).parse() {
        Ok(ok) => println!("{:?}", ok),
        Err(lexer::Error::EOF) => panic!(),
        Err(lexer::Error::Message(loc, msg)) => {
            println!(
                "{}:{}: {}",
                lexer.path_arena().borrow()[loc.file]
                    .as_path()
                    .display()
                    .to_string(),
                loc.line,
                msg
            );
            println!("{}", lexer.get_surrounding_line(loc));
        }
    }
    // loop {
    //     match lexer.get_token() {
    //         Ok(tok) => println!("{:?}", tok),
    //         Err(lexer::Error::Message(loc, msg)) => println!("error: {:?}: {}", loc, msg),
    //         Err(lexer::Error::EOF) => break,
    //     }
    // }
}
