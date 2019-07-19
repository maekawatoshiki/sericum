use cilk::{
    codegen::dag,
    // exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, function, module, types, value},
    *,
};

#[test]
fn dag1() {
    let mut m = module::Module::new("cilk");

    let func = cilk_ir!(m; define [i32] func () {
        entry:
            i = alloca i32;
            store (i32 123), (%i);
            l = load (%i);
            ret (%l);
    });

    println!("{}", m.function_ref(func).to_string(&m));

    dag::ConvertToDAG::new(&m).construct_dag(func);
}
