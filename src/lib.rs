pub mod ir;

extern crate id_arena;
extern crate rustc_hash;

#[cfg(test)]
mod tests {
    use crate::ir::{function, module, types};

    #[test]
    fn module() {
        let mut m = module::Module::new("cilk");
        let f_id = m.add_function(function::Function::new("f", types::Type::Int32, vec![]));
        let f = m.function_ref_mut(f_id);
        let bb_id = f.append_basic_block();
        f.dump();
    }
}
