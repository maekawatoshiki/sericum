use cilk::{
    exec::{interpreter::interp, jit::x64::compiler, jit::x64::liveness, jit::x64::regalloc},
    *,
};
use rustc_hash::FxHashMap;

#[test]
pub fn jit_fibo() {
    let mut m = module::Module::new("cilk");

    let fibo = cilk_ir!(m; define [i32] f (i32) {
        entry:
            cond = icmp le (%arg.0), (i32 2);
            br (%cond) l1, l2;
        l1:
            ret (i32 1);
        l2:
            a1 = sub (%arg.0), (i32 1);
            r1 = call f [(%a1)];
            a2 = sub (%arg.0), (i32 2);
            r2 = call f [(%a2)];
            r3 = add (%r1), (%r2);
            ret (%r3);
    });

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
