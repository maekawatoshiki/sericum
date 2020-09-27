extern crate cilkcc;
use cilkcc::compile;

extern crate argopt;
use argopt::cmd;

use std::path::PathBuf;

#[rustfmt::skip]
#[cmd]
fn main(
    /// Input files (*.c) 
    files: Vec<PathBuf>,
) {
    println!("cilkcc {:?}", files);
    compile(&files[0])
}
