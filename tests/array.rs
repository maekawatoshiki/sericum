use cilk::{
    exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, function, module, types, value},
    *,
};

#[test]
fn array1() {
    let mut m = module::Module::new("cilk");

    let func = cilk_ir!(m; define [i32] func () {
        entry:
            a = alloca_ ([8; i32]);

            idx = gep (%a), [(i32 0), (i32 3)];
            store (i32 123), (%idx);

            idx = gep (%a), [(i32 0), (i32 3)];
            l = load (%idx);
            ret (%l);
    });

    println!("{}", m.function_ref(func).to_string(&m));

    let mut interp = interp::Interpreter::new(&m);
    let ret = interp.run_function(func, vec![]);
    assert_eq!(ret, interp::ConcreteValue::Int32(123));

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    let ret = jit.run(func, vec![]);
    println!("{:?}", ret);
}
