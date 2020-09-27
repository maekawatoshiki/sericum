use std::collections::VecDeque;
use std::fs;
use std::path::PathBuf;

pub struct Lexer {
    sub_lexers: VecDeque<FileLexer>,
}

pub struct FileLexer {
    pub path: PathBuf,
    source: String,
    loc: SourceLoc,
}

#[derive(Debug, Clone, Copy)]
struct SourceLoc {
    pub line: usize,
    pub pos: usize,
}

impl Lexer {
    pub fn new(path: PathBuf) -> Self {
        Self {
            sub_lexers: {
                let mut sub_lexers = VecDeque::new();
                sub_lexers.push_back(FileLexer::new(path));
                sub_lexers
            },
        }
    }
}

impl FileLexer {
    pub fn new(path: PathBuf) -> Self {
        let source = fs::read_to_string(path.clone()).unwrap();
        Self {
            path,
            source,
            loc: SourceLoc::new(),
        }
    }

    pub fn get_char(&mut self) -> Option<char> {
        if self.loc.pos >= self.source.len() {
            return None;
        }
        self.source[self.loc.pos..].chars().next()
    }

    pub fn next_char(&mut self) -> Option<char> {
        if self.loc.pos >= self.source.len() {
            return None;
        }
        let c = self.source[self.loc.pos..].chars().next();
        self.loc.pos += 1;
        c
    }

    pub fn skip_char(&mut self, c: char) -> bool {
        if let Some(c_) = self.get_char() {
            return c_ == c;
        }
        false
    }
}

impl SourceLoc {
    pub fn new() -> Self {
        Self { line: 0, pos: 0 }
    }
}
