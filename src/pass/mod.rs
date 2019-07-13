pub mod dead_code_elimination;

use crate::ir::module::Module;

pub trait Pass {
    fn run(m: &Module);
}
