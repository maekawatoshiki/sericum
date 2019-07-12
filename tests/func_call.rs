use cilk::{
    exec::{jit::x64::compiler, jit::x64::liveness, jit::x64::regalloc},
    ir::{builder, function, module, types, value},
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

    liveness::LivenessAnalyzer::new(&m).analyze();
    regalloc::RegisterAllocator::new(&m).analyze();

    let f1 = m.function_ref(func1);
    println!("liveness analyzed:\n{}", f1.to_string(&m));

    let f2 = m.function_ref(func2);
    println!("liveness analyzed:\n{}", f2.to_string(&m));

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    let ret = jit.run(func2, vec![]);
    println!("jit: {:?}", ret);
}

#[test]
fn func_call2() {
    let mut m = module::Module::new("cilk");

    let f = cilk_ir!(m; define [i32] f (i32) {
        entry:
            r = add (%arg.0), (i32 1);
            ret (%r);
    });

    let main = cilk_ir!(m; define [i32] main (i32) {
        entry:
            i = alloca i32;
            store (i32 100), (%i);
            li = load (%i);
            a = call f [(%li)];
            ret (%a);
    });

    liveness::LivenessAnalyzer::new(&m).analyze();
    regalloc::RegisterAllocator::new(&m).analyze();

    println!("\n{}", m.function_ref(main).to_string(&m));
    println!("\n{}", m.function_ref(f).to_string(&m));

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    let ret = jit.run(main, vec![]);
    println!("jit: {:?}", ret);
}

#[test]
fn func_call3() {
    let mut m = module::Module::new("cilk");

    let f = cilk_ir!(m; define [i32] f (i32) {
        entry:
            _r = add (%arg.0), (i32 1); // actually, this line isn't needed (so eliminated)
            ret (i32 100);
    });

    let main = cilk_ir!(m; define [i32] main (i32) {
        entry:
            i = alloca i32;
            store (i32 100), (%i);
            li = load (%i);
            a = call f [(%li)];
            ret (%a);
    });

    liveness::LivenessAnalyzer::new(&m).analyze();
    regalloc::RegisterAllocator::new(&m).analyze();

    println!("\n{}", m.function_ref(main).to_string(&m));
    println!("\n{}", m.function_ref(f).to_string(&m));

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    let ret = jit.run(main, vec![]);
    println!("jit: {:?}", ret);
}
