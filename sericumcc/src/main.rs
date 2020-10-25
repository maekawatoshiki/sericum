extern crate sericumcc;
use sericumcc::compile;

extern crate argopt;
use argopt::cmd;

use std::path::PathBuf;

#[rustfmt::skip]
#[cmd]
fn main(
    /// Input files (*.c) 
    files: Vec<PathBuf>,
) {
    for file in files {
        compile(file)
    }
    ()
}

#[test]
fn run_examples() {
    use std::fs;
    let paths = match fs::read_dir("./examples") {
        Ok(paths) => paths,
        Err(e) => panic!("{:?}", e.kind()),
    };
    for path in paths {
        eprintln!("{:?}", path);
        let name = path.as_ref().unwrap().path().to_str().unwrap().to_string();
        if name.ends_with(".h") || name.contains("game_of_life") || name.contains("hello.c") {
            continue;
        }
        compile(path.unwrap().path())
    }
}
