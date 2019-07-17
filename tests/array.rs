use cilk::{
    exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, function, module, types, value},
    *,
};

#[test]
fn array1() {
    let mut m = module::Module::new("cilk");

    let func = m.add_function(function::Function::new("func", types::Type::Void, vec![]));
    let mut builder = builder::Builder::new(&mut m, func);

    let bb_entry = builder.append_basic_block();
    builder.set_insert_point(bb_entry);

    builder.build_alloca(types::Type::Array(Box::new(types::ArrayType::new(
        types::Type::Int32,
        8,
    ))));
    builder.build_ret(value::Value::None);

    println!(
        "{}",
        builder.module.function_ref(func).to_string(&builder.module)
    );

    let mut interp = interp::Interpreter::new(&m);
    interp.run_function(func, vec![]);
}
