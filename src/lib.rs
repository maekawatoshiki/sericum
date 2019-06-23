pub mod ir;

extern crate id_arena;
extern crate rustc_hash;

#[cfg(test)]
mod tests {
    use crate::ir::{function, module, types};

    #[test]
    fn module() {
        let mut m = module::Module::new("cilk");
        let f_id = m.add_function(function::Function::new(
            "f",
            types::Type::Void,
            vec![types::Type::Int32, types::Type::Int32],
        ));
        let f = m.function_ref_mut(f_id);
        let bb_id = f.append_basic_block();
        let bb = f.basic_block_ref_mut(bb_id);
        bb.build_alloca(types::Type::Int32);
        println!("{}", f.to_string());
    }
}
