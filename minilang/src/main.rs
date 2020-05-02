extern crate cilk;
mod codegen;
mod parser;

fn main() {
    let input = r#"
function main(a: i32): i32 {
    var arr: [16] i32;
    arr[a] = 1;
    return arr[a];
}
"#;
    let mut codegen = codegen::CodeGenerator::new();
    codegen.run(input);

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
