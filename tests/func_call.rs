use cilk::{
    exec::{interpreter::interp, jit::x64::compiler, jit::x64::liveness, jit::x64::regalloc},
    ir::{builder, function, module, opcode, types, value},
    *,
};

#[test]
fn func_call() {
    let mut m = module::Module::new("cilk");

    let func1 = cilk_ir!(m; define [i32] func1 (i32) {
        entry:
            a = add (%arg.0), (i32 1);
            ret (%a);
    });

    let func2 = cilk_ir!(m; define [i32] func2 () {
        entry:
            r = call func1 [(i32 10)];
            ret (%r);
    });

    let mut liveness = liveness::LivenessAnalyzer::new(&m);
    liveness.analyze();

    let f1 = m.function_ref(func1);
    println!("liveness analyzed:\n{}", f1.to_string(&m));

    let f2 = m.function_ref(func2);
    println!("liveness analyzed:\n{}", f2.to_string(&m));

    let mut regalloc = regalloc::RegisterAllocator::new(&m);
    regalloc.analyze();

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile(func1);
    jit.compile(func2);

    let ret = jit.run(func2, vec![]);
    println!("jit: {:?}", ret);
}
