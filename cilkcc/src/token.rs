use rustc_hash::FxHashSet;

pub struct Token {
    pub kind: TokenKind,
    pub leading_space: bool,
    hideset: FxHashSet<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {}
