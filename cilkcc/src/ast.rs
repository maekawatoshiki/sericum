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
    Char(char),
    String(String),
    UnaryOp(UnaryOp, Box<AST>),
    BinaryOp(BinaryOp, Box<AST>, Box<AST>),
    Assign { dst: Box<AST>, src: Box<AST> },
    Load(Box<AST>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    LogicalNot,
    BitwiseNot,
    Minus,
    // PreInc,
    // PreDec,
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

impl AST {
    pub fn new(kind: Kind, loc: SourceLoc) -> Self {
        Self { kind, loc }
    }

    pub fn eval(&self) -> Option<i64> {
        Some(match self.kind {
            Kind::Int { n, .. } => n,
            Kind::UnaryOp(UnaryOp::LogicalNot, ref e) => (e.eval()? == 0) as i64,
            Kind::UnaryOp(UnaryOp::BitwiseNot, ref e) => !e.eval()?,
            Kind::UnaryOp(UnaryOp::Minus, ref e) => -e.eval()?,
            Kind::UnaryOp(UnaryOp::PostInc, ref e) => e.eval()? + 1,
            Kind::UnaryOp(UnaryOp::PostDec, ref e) => e.eval()? - 1,
            Kind::UnaryOp(UnaryOp::Deref, ref e) => e.eval()?,
            Kind::UnaryOp(UnaryOp::Addr, ref e) => e.eval()?,
            Kind::BinaryOp(BinaryOp::Add, ref l, ref r) => l.eval()? + r.eval()?,
            Kind::BinaryOp(BinaryOp::Sub, ref l, ref r) => l.eval()? - r.eval()?,
            Kind::BinaryOp(BinaryOp::Mul, ref l, ref r) => l.eval()? * r.eval()?,
            Kind::BinaryOp(BinaryOp::Div, ref l, ref r) => l.eval()? / r.eval()?,
            Kind::BinaryOp(BinaryOp::Rem, ref l, ref r) => l.eval()? % r.eval()?,
            Kind::BinaryOp(BinaryOp::And, ref l, ref r) => l.eval()? & r.eval()?,
            Kind::BinaryOp(BinaryOp::Or, ref l, ref r) => l.eval()? | r.eval()?,
            Kind::BinaryOp(BinaryOp::Xor, ref l, ref r) => l.eval()? ^ r.eval()?,
            Kind::BinaryOp(BinaryOp::LogicalAnd, ref l, ref r) => l.eval()? & r.eval()?,
            Kind::BinaryOp(BinaryOp::LogicalOr, ref l, ref r) => l.eval()? | r.eval()?,
            Kind::BinaryOp(BinaryOp::Eq, ref l, ref r) => (l.eval()? == r.eval()?) as i64,
            Kind::BinaryOp(BinaryOp::Ne, ref l, ref r) => (l.eval()? != r.eval()?) as i64,
            Kind::BinaryOp(BinaryOp::Lt, ref l, ref r) => (l.eval()? < r.eval()?) as i64,
            Kind::BinaryOp(BinaryOp::Gt, ref l, ref r) => (l.eval()? > r.eval()?) as i64,
            Kind::BinaryOp(BinaryOp::Le, ref l, ref r) => (l.eval()? <= r.eval()?) as i64,
            Kind::BinaryOp(BinaryOp::Ge, ref l, ref r) => (l.eval()? >= r.eval()?) as i64,
            Kind::BinaryOp(BinaryOp::Shl, ref l, ref r) => l.eval()? << r.eval()?,
            Kind::BinaryOp(BinaryOp::Shr, ref l, ref r) => l.eval()? >> r.eval()?,
            Kind::BinaryOp(BinaryOp::Comma, ref l, ref r) => {
                l.eval()?;
                r.eval()?
            }
            _ => return None,
        })
    }
}
