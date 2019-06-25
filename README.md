# Cilk

[![CircleCI](https://circleci.com/gh/maekawatoshiki/cilk.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/cilk)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

(Toy) Compiler Infrastructure influenced by LLVM written in Rust.

# How It Looks Like

```rust
let mut m = Module::new("cilk");

let func_id = m.add_function(
                  Function::new(
                    "inc",
                    Type::Int32,
                    vec![Type::Int32]
                  )
              );
let func = m.function_ref_mut(func_id);

let mut builder = Builder::new(func);

let bb = builder.append_basic_block();
builder.set_insert_point(bb);

let arg0 = builder.get_param(0).unwrap();
let val = builder.build_add(
                      arg0, 
                      Value::Immediate(ImmediateValue::Int32(1))
          );
builder.build_ret(val);

println!("Dump function:\n{}", func.to_string());
// Dump function:
// define i32 inc(i32, ) {
// label0:
// %1 = add i32 %0, i32 1
// ret i32 %1
// }

let ret = Interpreter::new(&m)
                .run_function(func_id, vec![ConcreteValue::Int32(3)]);
println!("return value: {:?}", ret); // ConcreteValue::Int32(4)
