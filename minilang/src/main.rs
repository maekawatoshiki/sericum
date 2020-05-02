extern crate cilk;
mod codegen;
mod parser;

fn main() {
    let input = r#"
function main(a: i32): i32 {
    if a < 2 { return a; } 
    return a + 1;
}
"#;
    let mut codegen = codegen::CodeGenerator::new();
    codegen.run(input);
}
