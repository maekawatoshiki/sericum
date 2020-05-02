extern crate cilk;
mod codegen;
mod parser;

fn main() {
    let input = r#"
function main(a: i32): i32 {
    return a + 1;
}
"#;
    let mut codegen = codegen::CodeGenerator::new();
    codegen.run(input);
}
