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

    pub fn basic_block_ref_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.basic_blocks[id]
    }
}

impl Function {
    pub fn to_string(&self) -> String {
        format!(
            "define {} {}({}) {{\n{}}}",
            self.ret_ty.to_string(),
            self.name,
            self.params_ty.iter().fold("".to_string(), |mut s, p| {
                s += &(p.to_string() + ", ");
                s
            }),
            self.basic_blocks_to_string()
        )
    }

    fn basic_blocks_to_string(&self) -> String {
        self.basic_blocks.iter().fold("".to_string(), |s, (id, b)| {
            format!("{}label{}:\n{}\n", s, id.index(), b.to_string())
        })
    }
}
