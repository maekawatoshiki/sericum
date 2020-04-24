pub mod basic_block;
pub mod builder;
pub mod const_folding;
pub mod dom_tree;
pub mod function;
pub mod liveness;
pub mod mem2reg;
pub mod module;
pub mod opcode;
pub mod types;
pub mod value;

pub trait DumpToString {
    fn dump(&self, module: &module::Module) -> String;
}
