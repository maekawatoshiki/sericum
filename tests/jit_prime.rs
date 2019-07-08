use cilk::{
    exec::{interpreter::interp, jit::x64::compiler, jit::x64::liveness, jit::x64::regalloc},
    *,
};
use rustc_hash::FxHashMap;

#[test]
pub fn jit_prime() {
    let mut m = module::Module::new("cilk");

    /*
     * int prime(int n) {
     *     if (n == 2) return 1;
     *     if (n % 2 == 0) return 0;
     *     for (int i = 3; i * i <= n; i += 2) {
     *         if (n % i == 0) return 0;
     *     }
     *     return 1;
     * }
     */
    let fibo = cilk_ir!(m; define [i32] prime (i32) {
        entry:
            i = alloca i32;
            cond = icmp eq (%arg.0), (i32 2);
            br (%cond) l1, l2;
        l1:
            ret (i32 1);
        l2:
            r = rem (%arg.0), (i32 2);
            cond = icmp eq (%r), (i32 0);
            br (%cond) l3, l4;
        l3:
            ret (i32 0);
        l4:
            store (i32 3), (%i);
            br l5;
        l5:
            li = load (%i);
            m = mul (%li), (%li);
            cond = icmp le (%m), (%arg.0);
            br (%cond) l6, l7;
        l6:
            li = load (%i);
            r = rem (%arg.0), (%li);
            cond = icmp eq (%r), (i32 0);
            br (%cond) l8, l9;
        l8:
            ret (i32 0);
        l9:
            a = add (%li), (i32 2);
            store (%a), (%i);
            br l5;
        l7:
            ret (i32 1);
    });

    let mut liveness = liveness::LivenessAnalyzer::new(&m);
    liveness.analyze();

    let f = m.function_ref(fibo);
    println!("liveness: {}", f.to_string(&m));

    let mut regalloc = regalloc::RegisterAllocator::new(&m);
    regalloc.analyze();

    let mut interp = interp::Interpreter::new(&m);
    let ret = interp.run_function(fibo, vec![interp::ConcreteValue::Int32(97)]);
    println!("interp: prime(97) = {:?}", ret);
    let ret = interp.run_function(fibo, vec![interp::ConcreteValue::Int32(104)]);
    println!("interp: prime(104) = {:?}", ret);

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile(fibo);

    println!(
        "jit: prime(10009723) = {:?}",
        jit.run(fibo, vec![compiler::GenericValue::Int32(10009723)])
    );
    println!(
        "jit: prime(10009721) = {:?}",
        jit.run(fibo, vec![compiler::GenericValue::Int32(10009721)])
    );
}
