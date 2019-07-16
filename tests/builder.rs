use cilk::ir::{builder, function, module, types, value};

#[test]
fn builder() {
    let mut m = module::Module::new("cilk");

    let func = m.add_function(function::Function::new("func", types::Type::Int32, vec![]));
    let mut builder = builder::Builder::new(&mut m, func);

    let bb_entry = builder.append_basic_block();

    builder.set_insert_point(bb_entry);
    builder.build_ret(value::Value::Immediate(value::ImmediateValue::Int32(123)));

    builder.set_insert_point_at(0, bb_entry);
    builder.build_alloca(types::Type::Int32);

    println!("{}", m.function_ref(func).to_string(&m));
}
