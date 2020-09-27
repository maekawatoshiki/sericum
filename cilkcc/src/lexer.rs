use std::fs;
use std::path::PathBuf;

pub struct Lexer<'a> {
    path: &'a PathBuf,
    source: String,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(path: &'a PathBuf) -> Self {
        let source = fs::read_to_string(path).unwrap();
        Self {
            path,
            source,
            pos: 0,
        }
    }
}
