# Cilk

[![CircleCI](https://circleci.com/gh/maekawatoshiki/cilk.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/cilk)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

(Toy) Compiler Infrastructure influenced by LLVM written in Rust.

# Example

```rust
let mut m = module::Module::new("cilk");
let fibo = m.add_function(Function::new(
    "fibo",
    types::Type::Int32,
    vec![types::Type::Int32],
));
let mut builder = builder::Builder::new(&mut m, fibo);

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

let f = m.function_ref(fibo);
println!("Function dump:\n{}", f.to_string(&m));

// define i32 fibo(i32, ) {       
// label.0:                      
//     %0 = icmp le i32 %arg.0, i32 2
//     br i1 %0 %label.1, %label.2
        
// label.1:       
//     ret i32 1
        
// label.2:       
//     %3 = sub i32 %arg.0, i32 1
//     %4 = call i32 fibo(i32 %3, )
//     %5 = sub i32 %arg.0, i32 2                   
//     %6 = call i32 fibo(i32 %5, )
//     %7 = add i32 %4, i32 %6
//     ret i32 %7
                     
// }                               

let mut interp = interp::Interpreter::new(&m);
let ret = interp.run_function(fibo, vec![interp::ConcreteValue::Int32(10)]);

println!("fibo(10) = {:?}", ret); // fibo(10) = 55
```
