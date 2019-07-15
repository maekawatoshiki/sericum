use cilk::{
    exec::{jit::x64::compiler},
    *,
};
use rustc_hash::FxHashMap;

#[test]
pub fn jit_println() {
    let mut m = module::Module::new("cilk");

    // Internal function must be defined when you use it
    let cilk_println_i32 = m.add_function(ir::function::Function::new(
        "cilk.println.i32",
        ir::types::Type::Void,
        vec![ir::types::Type::Int32],
    ));

    let main = cilk_ir!(m; define [i32] main () {
        entry:
            i = alloca i32;
            store (i32 17), (%i);
            li = load (%i);
            __ = call (->cilk_println_i32) [(%li)];
            lj = load (%i);

            ret (%lj);
    });

    let mut jit = compiler::JITCompiler::new(&m);
    jit.compile_module();

    println!("liveness: {}", m.function_ref(main).to_string(&m));

    match jit.run(main, vec![]) {
        compiler::GenericValue::Int32(v) if v == 17 => {},
        compiler::GenericValue::Int32(v) => panic!("returned:{} expected: 17", v),
        x => panic!("returned:{:?} expected: Int(17)", x)
    }
}
