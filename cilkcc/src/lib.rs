extern crate cilk;
extern crate id_arena;
extern crate rustc_hash;

use std::path::PathBuf;
use std::{
    fs,
    io::{BufWriter, Write},
    process,
};
use {rand, rand::Rng};

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod types;

// TODO: Refine code

pub fn compile(path: PathBuf) {
    let mut lexer = lexer::Lexer::new(path);
    let mut parser = parser::Parser::new(&mut lexer);
    let nodes = match parser.parse() {
        Ok(ok) => ok,
        Err(lexer::Error::EOF) => panic!("unexpected EOF"),
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

    println!("{:#?}", nodes);

    let mut codegen = codegen::Codegenerator::new(&mut parser.compound_types);
    for node in nodes {
        if let Err(codegen::Error::Message(loc, msg)) = codegen.generate(&node) {
            println!(
                "{}:{}: {}",
                parser.lexer.path_arena().borrow()[loc.file]
                    .as_path()
                    .display()
                    .to_string(),
                loc.line,
                msg
            );
            println!("{}", parser.lexer.get_surrounding_line(loc));
            panic!();
        }
    }
    println!("{:?}", codegen.module);

    let machine_module =
        cilk::codegen::x64::standard_conversion_into_machine_module(&mut codegen.module);
    let mut printer = cilk::codegen::x64::asm::print::MachineAsmPrinter::new();
    printer.run_on_module(&machine_module);
    println!("{}", printer.output);

    assemble_and_run("int main(int argc, char **argv);", &printer.output);
}

fn unique_file_name(extension: &str) -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                            abcdefghijklmnopqrstuvwxyz\
                            0123456789";
    const LEN: usize = 16;
    let mut rng = rand::thread_rng();
    let name: String = (0..LEN)
        .map(|_| {
            let idx = rng.gen_range(0, CHARSET.len());
            CHARSET[idx] as char
        })
        .collect();
    format!("/tmp/{}.{}", name, extension)
}

fn assemble_and_run(c_parent: &str, s_target: &str) {
    let parent_name = unique_file_name("c");
    let target_name = unique_file_name("s");
    {
        let mut parent = BufWriter::new(fs::File::create(parent_name.as_str()).unwrap());
        let mut target = BufWriter::new(fs::File::create(target_name.as_str()).unwrap());
        parent.write_all(c_parent.as_bytes()).unwrap();
        target.write_all(s_target.as_bytes()).unwrap();
    }

    let output_name = unique_file_name("out");
    let compilation = process::Command::new("gcc")
        .args(&[
            parent_name.as_str(),
            target_name.as_str(),
            "-o",
            output_name.as_str(),
        ])
        .status()
        .unwrap();
    assert!(compilation.success());

    let execution = process::Command::new(output_name.as_str())
        .status()
        .unwrap();
    if let Some(code) = execution.code() {
        println!("Exit code: {:?}", code);
    } else {
        assert!(execution.success());
    }

    fs::remove_file(output_name).unwrap();
    fs::remove_file(parent_name).unwrap();
    fs::remove_file(target_name).unwrap();
}
