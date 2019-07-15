use cilk::{
    exec::jit::x64::compiler,
    ir::{builder, function, module, types, value},
    *,
};

#[test]
fn func_call1() {
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

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    println!("{}", m.function_ref(func1).to_string(&m));
    println!("{}", m.function_ref(func2).to_string(&m));

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

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    println!("{}", m.function_ref(main).to_string(&m));
    println!("{}", m.function_ref(f).to_string(&m));

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

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    println!("{}", m.function_ref(main).to_string(&m));
    println!("{}", m.function_ref(f).to_string(&m));

    let ret = jit.run(main, vec![]);
    println!("jit: {:?}", ret);
    assert_eq!(ret, compiler::GenericValue::Int32(100));
}

#[test]
fn func_call4() {
    let mut m = module::Module::new("cilk");

    // Internal function must be defined when you use it
    let cilk_println_i32 = m.add_function(ir::function::Function::new(
        "cilk.println.i32",
        types::Type::Void,
        vec![types::Type::Int32],
    ));

    let main = cilk_ir!(m; define [void] main () {
        entry:
            i = alloca i32;
            store (i32 10), (%i);
            li = load (%i);
            __ = call (->cilk_println_i32) [(%li)];    // If remove this line, this code works.
            ret (void);
    });

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();
    jit.run(main, vec![]);
}

#[test]
fn func_call5() {
    let mut m = module::Module::new("cilk");

    let f = cilk_ir!(m; define [i32] f (i32, i32) {
        entry:
            i = sub (%arg.0), (%arg.1);
            ret (%i);
    });

    let main = cilk_ir!(m; define [i32] main () {
        entry:
            i = call f [(i32 1), (i32 2)];
            ret (%i);
    });

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    println!("{}", m.function_ref(f).to_string(&m));

    let ret = jit.run(main, vec![]);
    assert_eq!(ret, compiler::GenericValue::Int32(-1));
}
