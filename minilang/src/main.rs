extern crate cilk;
mod codegen;
mod parser;

fn main() {
    //     let pi_input = r#"
    // function main(): i32 {
    //     var a: i32; a = 10000;
    //     var c: i32; c = 8400;
    //     var b: i32;
    //     var d: i32;
    //     var e: i32;
    //     var g: i32;
    //     var f: [8401] i32;
    //
    //     b = 0;
    //     while b < c {
    //         f[b] = a / 5;
    //         b = b + 1;
    //     }
    //
    //     e = 0;
    //     c = 8400;
    //     while 0 < c {
    //         d = 0;
    //         b = c - 1;
    //         while 0 < b {
    //             g = b * 2- 1;
    //             d = d * b + f[b] * a;
    //             f[b] = d % g;
    //             d = d / g;
    //             b = b - 1;
    //         }
    //
    //         println_i32(e + d / a);
    //
    //         e = d % a;
    //         c = c - 14;
    //     }
    //
    //     return 0;
    // }
    // "#;
    let input = r#"
    struct A {
        first: [8][8] i32,
        second: i32
    }
    function f(): i32 {
        println_i32(2);
        return  0;
    }
    function main(): i32 {
        f();
        return 0;
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
            vec![
            // cilk::codegen::x64::exec::jit::GenericValue::Int32(10)
            ],
        )
    );
}
