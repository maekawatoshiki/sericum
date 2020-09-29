use super::token::SourceLoc;

#[derive(Debug, Clone)]
pub struct AST {
    pub kind: Kind,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub enum Kind {}
