use id_arena::Id;
use rustc_hash::FxHashSet;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub leading_space: bool,
    pub loc: SourceLoc,
    pub hideset: FxHashSet<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    MacroParam { nth: usize },
    Keyword(Keyword),
    Identifier(String),
    Int { n: i64, bits: u8 },
    Float(f64),
    String(String),
    Char(char),
    Symbol(Symbol),
    Newline,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Typedef,
    Extern,
    Static,
    Auto,
    Restrict,
    Register,
    Const,
    ConstExpr,
    Volatile,
    Void,
    Signed,
    Unsigned,
    Char,
    Int,
    Short,
    Long,
    Float,
    Double,
    Struct,
    Enum,
    Union,
    Noreturn,
    Inline,
    If,
    Else,
    For,
    Do,
    While,
    Switch,
    Case,
    Default,
    Goto,
    Break,
    Continue,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    OpeningParen,
    ClosingParen,
    OpeningBrace,
    ClosingBrace,
    OpeningBoxBracket,
    ClosingBoxBracket,
    Comma,
    Semicolon,
    Colon,
    Point,
    Arrow,
    Inc,
    Dec,
    Add,
    Sub,
    Asterisk,
    Div,
    Mod,
    Not,
    BitwiseNot,
    Ampersand,
    Shl,
    Shr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Xor,
    Or,
    LAnd,
    LOr,
    Question,
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignShl,
    AssignShr,
    AssignAnd,
    AssignXor,
    AssignOr,
    Hash,
    Vararg,
    Sizeof,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct SourceLoc {
    pub file: Id<PathBuf>,
    pub line: usize,
    pub pos: usize,
}

impl Token {
    pub fn new(kind: TokenKind, loc: SourceLoc) -> Self {
        Self {
            kind,
            leading_space: false,
            loc,
            hideset: FxHashSet::default(),
        }
    }

    pub fn leading_space(mut self, x: bool) -> Self {
        self.leading_space = x;
        self
    }
}
