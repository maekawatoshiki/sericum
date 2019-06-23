use super::{basic_block::*, types::*};
use id_arena::*;

pub type BasicBlockId = Id<BasicBlock>;

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

impl Function {
    pub fn new(name: &str, ret_ty: Type, params_ty: Vec<Type>) -> Self {
        Self {
            name: name.to_string(),
            ret_ty,
            params_ty,
            basic_blocks: Arena::new(),
        }
    }

    pub fn append_basic_block(&mut self) -> BasicBlockId {
        self.basic_blocks.alloc(BasicBlock::new())
    }
}

impl Function {
    pub fn dump(&self) {
        eprintln!("define {} {}", self.ret_ty.to_string(), self.name);
    }
}
