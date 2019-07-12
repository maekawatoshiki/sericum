use cilk::{
    exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, function, module, opcode, types, value},
};

#[test]
pub fn sum() {
    let mut m = module::Module::new("cilk");

    let sum = m.add_function(function::Function::new(
        "sum",
        types::Type::Int32,
        vec![types::Type::Int32],
    ));
    let mut builder = builder::Builder::new(&mut m, sum);

    /*
     * int sum(int max) {
     *     int i = 1;
     *     int total = 0;
     *     while (i <= max) {
     *         total += i;
     *         i += 1;
     *     }
     *     return total;
     * }
     */
    let entry = builder.append_basic_block();
    let br_cond = builder.append_basic_block();
    let br_loop = builder.append_basic_block();
    let br_merge = builder.append_basic_block();
    builder.set_insert_point(entry);
    let var_i = builder.build_alloca(types::Type::Int32);
    let var_total = builder.build_alloca(types::Type::Int32);
    builder.build_store(
        value::Value::Immediate(value::ImmediateValue::Int32(1)),
        var_i,
    );
    builder.build_store(
        value::Value::Immediate(value::ImmediateValue::Int32(0)),
        var_total,
    );
    builder.build_br(br_cond);
    builder.set_insert_point(br_cond);
    builder.set_insert_point(br_cond);
    let arg0 = builder.get_param(0).unwrap();
    let val_i = builder.build_load(var_i);
    let cmp = builder.build_icmp(opcode::ICmpKind::Le, val_i, arg0);
    builder.build_cond_br(cmp, br_loop, br_merge);
    builder.set_insert_point(br_loop);
    let val_total = builder.build_load(var_total);
    let val_i = builder.build_load(var_i);
    let add_total = builder.build_add(val_total, val_i);
    builder.build_store(add_total, var_total);
    let add_i = builder.build_add(
        val_i,
        value::Value::Immediate(value::ImmediateValue::Int32(1)),
    );
    builder.build_store(add_i, var_i);
    builder.build_br(br_cond);
    builder.set_insert_point(br_merge);
    let val_total = builder.build_load(var_total);
    builder.build_ret(val_total);

    let mut interp = interp::Interpreter::new(&m);
    let ret = interp.run_function(sum, vec![interp::ConcreteValue::Int32(10)]);
    println!("exec: sum(10) = {:?}", ret);
    assert_eq!(ret, interp::ConcreteValue::Int32(55));

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    println!("{}", m.function_ref(sum).to_string(&m));

    let ret = jit.run(sum, vec![compiler::GenericValue::Int32(10)]);
    println!("jit: sum(10) = {:?}", ret);
    assert_eq!(ret, compiler::GenericValue::Int32(55));
}
