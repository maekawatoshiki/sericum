use super::basic_block::BasicBlocksTrait;

pub trait FunctionTrait {
    type BBS: BasicBlocksTrait;
    fn get_basic_blocks(&self) -> &Self::BBS;
}
