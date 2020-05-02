extern crate cilk;
mod codegen;
mod parser;

fn main() {
    let input = r#"
        function main(a: i32, b: i32): i32 {
            return a + b;
        }
        "#;
    let mut codegen = codegen::CodeGenerator::new();
    codegen.generate(input);
}
