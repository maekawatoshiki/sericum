extern crate cilk;
mod parser;

fn main() {
    let r = parser::parser::function(
        r#"
        function main(a: i32, b: i32): i32 {
            return a + b;
        }
        "#,
    );
    println!("{:?}", r);
    println!("Hello, world!");
}
