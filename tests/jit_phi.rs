use cilk::{
    exec::interpreter::interp,
    exec::jit::x64::compiler,
    ir::{builder, function, module, types, value},
    *,
};

#[test]
fn jit_phi() {
    let mut m = module::Module::new("cilk");

    /*
     * int func1(int n) {
     *     if (n == 2) return 1;
     *     int a = n + 2;
     *     if (a == 3) {
     *         return a; // 3
     *     } else {
     *         return a * 3;
     *     }
     * }
     */
    let func1 = cilk_ir!(m; define [i32] func1 (i32) {
        entry:
            c = icmp eq (%arg.0), (i32 2);
            br (%c) l1, l2;
        l1:
            br merge;
        l2:
            a = add (%arg.0), (i32 2);
            c = icmp eq (%a), (i32 3);
            br (%c) l3, merge;
        l3:
            b = mul (%a), (i32 3);
            br merge;
        merge:
            p = phi [ [(i32 1), l1], [(%a), l2], [(%b), l3] ];
            ret (%p);
    });

    println!("{}", m.function_ref(func1).to_string(&m));

    let mut interp = interp::Interpreter::new(&m);
    let ret = interp.run_function(func1, vec![interp::ConcreteValue::Int32(2)]);
    assert_eq!(ret, interp::ConcreteValue::Int32(1));
    let ret = interp.run_function(func1, vec![interp::ConcreteValue::Int32(3)]);
    assert_eq!(ret, interp::ConcreteValue::Int32(5));
    let ret = interp.run_function(func1, vec![interp::ConcreteValue::Int32(1)]);
    assert_eq!(ret, interp::ConcreteValue::Int32(9));

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    let ret = jit.run(func1, vec![compiler::GenericValue::Int32(2)]);
    println!("{:?}", ret);
    assert_eq!(ret, compiler::GenericValue::Int32(1));
    let ret = jit.run(func1, vec![compiler::GenericValue::Int32(3)]);
    println!("{:?}", ret);
    assert_eq!(ret, compiler::GenericValue::Int32(5));
    let ret = jit.run(func1, vec![compiler::GenericValue::Int32(1)]);
    println!("{:?}", ret);
    assert_eq!(ret, compiler::GenericValue::Int32(9));
}
