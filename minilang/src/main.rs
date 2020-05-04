extern crate cilk;
mod codegen;
mod parser;

fn main() {
    // let input = r#"
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
    // let input = r#"
    // struct A {
    //     first: [8][8] i32,
    //     second: i32
    // }
    // function f(): i32 {
    //     println_i32(2);
    //     return  0;
    // }
    // function main(): i32 {
    //     f();
    //     return 0;
    // }
    // "#;
    // let input = r#"
    // function m(c_x: f64, c_y: f64, n: i32): i32 {
    //     var x_n: f64; x_n = 0.0;
    //     var y_n: f64; y_n = 0.0;
    //     var x_n_1: f64; var y_n_1: f64;
    //     var i: i32;
    //     i = 0;
    //     while i < n {
    //         x_n_1 = x_n*x_n - y_n*y_n + c_x;
    //         y_n_1 = x_n * y_n * 2.0 + c_y;
    //         if 4.0 < x_n_1*x_n_1 + y_n_1*y_n_1 {
    //             return n;
    //         } else {
    //             x_n = x_n_1;
    //             y_n = y_n_1;
    //         }
    //         i = i + 1;
    //     }
    //     return 0;
    // }
    // function main(): i32 {
    //     var x_max: f64; x_max = 1.0;
    //     var x_min: f64; x_min = 0.0 - 2.0;
    //     var y_max: f64; y_max = 1.0;
    //     var y_min: f64; y_min = 0.0 - 1.0;
    //     var dx: f64; dx = 0.05;
    //     var dy: f64; dy = 0.05;
    //     var y: f64; var x: f64;
    //     y = y_max;
    //     while y_min < y {
    //         x = x_min;
    //         while x < x_max {
    //             if m(x, y, 300) == 0 {
    //                 printch_i32(65);
    //             } else {
    //                 printch_i32(32);
    //             }
    //             x = x + dx;
    //         }
    //         printch_i32(10);
    //         y = y - dy;
    //     }
    //     return 0;
    // }
    //     "#;
    let input = "
    function main(): i32 {
        println_f64( sin(3.14 / 2.0) );
        println_f64( cos(3.14 / 2.0) );
        println_f64( sqrt(2.0) );
        return 0;
    }
    ";
    let mut codegen = codegen::CodeGenerator::new();
    codegen.run(input);

    let mut jit = cilk::codegen::x64::exec::jit::JITExecutor::new(&codegen.module);
    let func = jit.find_function_by_name("main").unwrap();
    println!("Result: {:?}", jit.run(func, vec![]));
}
