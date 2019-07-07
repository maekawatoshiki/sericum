use cilk::{
    exec::{interpreter::interp, jit::x64::compiler, jit::x64::liveness, jit::x64::regalloc},
    ir::{builder, function, module, opcode, types, value},
};

#[test]
pub fn jit_fibo() {
    let mut m = module::Module::new("cilk");

    let fibo = m.add_function(function::Function::new(
        "fbo",
        types::Type::Int32,
        vec![types::Type::Int32],
    ));
    let mut builder = builder::Builder::new(&mut m, fibo);

    // fibonacci
    let entry = builder.append_basic_block();
    let br1 = builder.append_basic_block();
    let br2 = builder.append_basic_block();
    builder.set_insert_point(entry);
    let arg0 = builder.get_param(0).unwrap();
    let eq1 = builder.build_icmp(
        opcode::ICmpKind::Le,
        arg0,
        value::Value::Immediate(value::ImmediateValue::Int32(2)),
    );
    builder.build_cond_br(eq1, br1, br2);
    builder.set_insert_point(br1);
    builder.build_ret(value::Value::Immediate(value::ImmediateValue::Int32(1)));
    builder.set_insert_point(br2);
    let fibo1arg = builder.build_sub(
        arg0,
        value::Value::Immediate(value::ImmediateValue::Int32(1)),
    );
    let fibo1 = builder.build_call(value::Value::Function(fibo), vec![fibo1arg]);
    let fibo2arg = builder.build_sub(
        arg0,
        value::Value::Immediate(value::ImmediateValue::Int32(2)),
    );
    let fibo2 = builder.build_call(value::Value::Function(fibo), vec![fibo2arg]);
    let add = builder.build_add(fibo1, fibo2);
    builder.build_ret(add);

    let mut liveness = liveness::LivenessAnalyzer::new(&m);
    liveness.analyze();

    let f = m.function_ref(fibo);
    println!("liveness: {}", f.to_string(&m));

    let mut regalloc = regalloc::RegisterAllocator::new(&m);
    regalloc.analyze();

    let mut interp = interp::Interpreter::new(&m);
    let ret = interp.run_function(fibo, vec![interp::ConcreteValue::Int32(9)]);
    println!("exec: fibo(9) = {:?}", ret);

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile(fibo);
    println!(
        "jit: fibo(9) = {:?}",
        jit.run(fibo, vec![compiler::GenericValue::Int32(9)])
    );
    println!(
        "jit: fibo(40) = {:?}",
        jit.run(fibo, vec![compiler::GenericValue::Int32(40)])
    );
}
