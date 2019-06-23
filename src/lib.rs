pub mod ir;

extern crate id_arena;
extern crate rustc_hash;

#[cfg(test)]
mod tests {
    use crate::ir::{builder, function, module, types, value};

    #[test]
    fn module() {
        let mut m = module::Module::new("cilk");
        let f = {
            let f_id = m.add_function(function::Function::new(
                "f",
                types::Type::Void,
                vec![types::Type::Int32, types::Type::Int32],
            ));
            m.function_ref_mut(f_id)
        };
        let mut builder = builder::Builder::new(f);
        let bb = builder.append_basic_block();
        builder.set_insert_point(bb);
        let var = builder.build_alloca(types::Type::Int32);
        let val = builder.build_load(var);
        builder.build_add(
            val,
            value::Value::Immediate(value::ImmediateValue::Int32(3)),
        );
        println!("{}", f.to_string());
    }
}
