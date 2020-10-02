extern crate cilk;
extern crate id_arena;
extern crate rustc_hash;

use std::path::PathBuf;

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod types;

pub fn compile(path: PathBuf) {
    let mut lexer = lexer::Lexer::new(path);
    let mut parser = parser::Parser::new(&mut lexer);
    let nodes = match parser.parse() {
        Ok(ok) => ok,
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
            panic!();
        }
    };
    println!("{:?}", nodes);
    let mut codegen = codegen::Codegenerator::new(&mut parser.compound_types);
    for node in nodes {
        codegen.generate(&node);
    }
    println!("{:?}", codegen.module);
}
