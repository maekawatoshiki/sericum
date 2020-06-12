# Cilk

[![CircleCI](https://circleci.com/gh/maekawatoshiki/cilk.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/cilk)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Toy Compiler Infrastructure influenced by LLVM written in Rust.

Do not expect too much stuff!


# To Do

- Optimization
    - Easy
        1. Spill registers not to the stack but to callee saved registers such as ebx. (llc does so)
        2. Take into consideration the physical registers' allocation order.
    - Hard 
        1. ....
- Refine code 
- Write documents in detail
- Write tests (because I recently removed most of them)

# Build

**Requirement: Rust nightly**

```sh
cargo test --feature x86_64 # build for x86_64
cargo test brainfuxk --feature x86_64 -- --nocapture # this is fun. just try it.
cargo test --feature riscv64 # build for riscv64. very few features are implemented.
```

# Example

- Fibonacci (**the following code may not work. take a look at ./tests**)

```rust
use cilk::{
    codegen::x64::{exec},
    exec::interpreter::interp,
    ir::{builder::Builder, module::Module, value::{Value, ImmediateValue}, types::Type},
};

let mut m = Module::new("cilk");
let fibo = m.create_function(
    "fibo", Type::Int32, vec![Type::Int32]
);
let mut builder = Builder::new(&mut m, fibo);

{
let entry = builder.append_basic_block();
let br1 = builder.append_basic_block();
let br2 = builder.append_basic_block();

builder.set_insert_point(entry);
    let arg0 = builder.get_param(0).unwrap();
    let eq1 = builder.build_icmp(
        ICmpKind::Le,
        arg0,
        Value::Immediate(ImmediateValue::Int32(2)),
    );
    builder.build_cond_br(eq1, br1, br2);

builder.set_insert_point(br1);
    builder.build_ret(Value::Immediate(ImmediateValue::Int32(1)));
 
builder.set_insert_point(br2);
    let fibo1arg = builder.build_sub(
        arg0,
        Value::Immediate(ImmediateValue::Int32(1)),
    );
    let fibo1 = builder.build_call(Value::Function(fibo), vec![fibo1arg]);
    let fibo2arg = builder.build_sub(
        arg0,
        Value::Immediate(ImmediateValue::Int32(2)),
    );
    let fibo2 = builder.build_call(Value::Function(fibo), vec![fibo2arg]);
    let add = builder.build_add(fibo1, fibo2);
    builder.build_ret(add);

println!("Function dump:\n{}", m.dump(fibo));
}

// Function dump:
// define i32 fibo(i32) {       
// label.0:                      
//     %0 = icmp le i32 %arg.0, i32 2
//     br i1 %0 %label.1, %label.2
//        
// label.1:       
//     ret i32 1
//        
// label.2:       
//     %3 = sub i32 %arg.0, i32 1
//     %4 = call i32 fibo(i32 %3, )
//     %5 = sub i32 %arg.0, i32 2                   
//     %6 = call i32 fibo(i32 %5, )
//     %7 = add i32 %4, i32 %6
//     ret i32 %7
//                     
// }                               

// In this branch, this may not work correctly.
let mut interp = interp::Interpreter::new(&m);
let ret = interp.run_function(fibo, vec![interp::ConcreteValue::Int32(10)]);
println!("fibo(10) = {:?}", ret); // fibo(10) = Int32(55)

// JIT suppports for only x86_64

let mut jit = exec::jit::JITExecutor::new(&m);
let func = jit.find_function_by_name("func").unwrap();
println!( "fibo(10) = {:?}",
          jit.run(func, vec![exec::jit::GenericValue::Int32(10)])); // fibo(10) = 55
```

- Useful macro to describe IR is available

```rust
let fibo = cilk_ir!(m; define [i32] f (i32) {
    entry:
        cond = icmp le (%arg.0), (i32 2);
        br (%cond) l1, l2;
    l1:
        ret (i32 1);
    l2:
        a1 = sub (%arg.0), (i32 1);
        r1 = call f [(%a1)];
        a2 = sub (%arg.0), (i32 2);
        r2 = call f [(%a2)];
        r3 = add (%r1), (%r2);
        ret (%r3);
});
```
