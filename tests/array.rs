use cilk::{
    exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, function, module, types, value},
};

#[test]
fn array1() {
    let mut m = module::Module::new("cilk");

    let func = m.add_function(function::Function::new("func", types::Type::Int32, vec![]));
    let mut builder = builder::Builder::new(&mut m, func);

    let bb_entry = builder.append_basic_block();
    builder.set_insert_point(bb_entry);

    let v = builder.build_alloca(types::Type::Array(Box::new(types::ArrayType::new(
        types::Type::Int32,
        8,
    ))));
    let i = builder.build_gep(
        v,
        vec![
            value::Value::Immediate(value::ImmediateValue::Int32(0)),
            value::Value::Immediate(value::ImmediateValue::Int32(2)),
        ],
    );
    builder.build_store(
        value::Value::Immediate(value::ImmediateValue::Int32(123)),
        i,
    );
    let li = builder.build_load(i);
    builder.build_ret(li);

    println!(
        "{}",
        builder.module.function_ref(func).to_string(&builder.module)
    );

    let mut interp = interp::Interpreter::new(&m);
    let ret = interp.run_function(func, vec![]);
    assert_eq!(ret, interp::ConcreteValue::Int32(123));

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    let ret = jit.run(func, vec![]);
    println!("{:?}", ret);
}
