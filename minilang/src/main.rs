extern crate cilk;
mod codegen;
mod parser;

fn main() {
    let input = r#"
function main(a: i32): i32 {
    var i: i32; i = 1;
    var total: i32; total = 0;
    while i <= a { total = total + i; i = i + 1; }
    return total;
}
"#;
    let mut codegen = codegen::CodeGenerator::new();
    codegen.run(input);

    cilk::ir::mem2reg::Mem2Reg::new().run_on_module(&mut codegen.module);
    let mut jit = cilk::codegen::x64::exec::jit::JITExecutor::new(&codegen.module);
    let func = jit.find_function_by_name("main").unwrap();
    println!(
        "Result: {:?}",
        jit.run(
            func,
            vec![cilk::codegen::x64::exec::jit::GenericValue::Int32(10)],
        )
    );
}
