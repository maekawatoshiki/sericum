pub mod exec;
pub mod ir;

extern crate id_arena;
extern crate rustc_hash;

#[cfg(test)]
mod tests {
    use crate::{
        exec::interpreter::interp,
        ir::{builder, function, module, opcode, types, value},
    };

    #[test]
    fn interpret() {
        let mut m = module::Module::new("cilk");

        let f_id = m.add_function(function::Function::new(
            "f",
            types::Type::Int32,
            vec![types::Type::Int32],
        ));
        let f = m.function_ref_mut(f_id);

        let mut builder = builder::Builder::new(f);
        let bb = builder.append_basic_block();
        let bb2 = builder.append_basic_block();
        builder.set_insert_point(bb);
        let var = builder.build_alloca(types::Type::Int32);
        let val = builder.build_load(var);
        let val2 = builder.build_add(
            val,
            value::Value::Immediate(value::ImmediateValue::Int32(1)),
        );
        builder.build_br(bb2);
        builder.set_insert_point(bb2);
        let arg0 = builder.get_param(0).unwrap();
        let val3 = builder.build_add(val2, arg0);
        let eq = builder.build_icmp(
            opcode::ICmpKind::Eq,
            val3,
            value::Value::Immediate(value::ImmediateValue::Int32(4)),
        );
        builder.build_ret(val3);

        println!("{}", f.to_string());

        let ret =
            interp::Interpreter::new(&m).run_function(f_id, vec![interp::ConcreteValue::Int32(3)]);
        assert_eq!(ret, interp::ConcreteValue::Int32(4));

        println!("exec: ret: {:?}", ret);
    }
}
