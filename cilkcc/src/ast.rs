use super::token::SourceLoc;

#[derive(Debug, Clone)]
pub struct AST {
    pub kind: Kind,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Int { n: i64, bits: u8 },
    Float(f64),
    Char(u8),
    String(String),
    UnaryOp(UnaryOp, Box<AST>),
    BinaryOp(BinaryOp, Box<AST>, Box<AST>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    LogicalNot,
    BitwiseNot,
    Minus,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    Deref,
    Addr,
    Sizeof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    LogicalAnd,
    LogicalOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Comma,
    // Assign,
}
