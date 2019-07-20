use cilk::{
    codegen::dag,
    // exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, function, module, types, value},
    *,
};

#[test]
fn dag1() {
    let mut m = module::Module::new("cilk");

    let func = cilk_ir!(m; define [i32] func (i32) {
        entry:
            i = alloca i32;
            store (%arg.0), (%i);
            br l1;
        l1:
            x = load (%i);
            y = add (%x), (i32 1);
            ret (%y);
    });

    println!("{}", m.function_ref(func).to_string(&m));

    dag::ConvertToDAG::new(&m).construct_dag(func);
}
