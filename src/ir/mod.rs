pub mod basic_block;
pub mod builder;
pub mod codegen_prepare;
pub mod const_folding;
pub mod cse;
pub mod function;
pub mod global_val;
pub mod licm;
pub mod liveness;
pub mod mem2reg;
pub mod merge_ret;
pub mod module;
pub mod opcode;
pub mod simplify_loop;
pub mod types;
pub mod value;

pub trait DumpToString {
    fn dump(&self, module: &module::Module) -> String;
}
