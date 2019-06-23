use super::{basic_block::*, types::*};
use id_arena::*;

#[derive(Debug, Clone)]
pub struct Function {
    /// Function name
    pub name: String,

    /// Function returning type
    pub ret_ty: Type,

    /// Function parameters type
    pub params_ty: Vec<Type>,

    /// Basic blocks
    pub basic_blocks: Arena<BasicBlock>,
}
