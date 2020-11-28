# Sericum

[![CircleCI](https://circleci.com/gh/maekawatoshiki/sericum.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/sericum)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)
[![sericum at docs.rs](https://docs.rs/sericum/badge.svg)](https://docs.rs/sericum)

(Toy) Compiler Infrastructure influenced by LLVM written in Rust

Do not expect too much stuff!

# To Do

- [ ] Implement basic block parameters
- [ ] Make it possible to generate code for multiple targets without rebuilding sericum itself
- [ ] Verify IR
- [ ] More optimizations for IR
- [ ] Support returning struct as value
- [ ] Write documents

# Build

**Requirement: Rust nightly**

```sh
cargo test           --features x86_64                          # build for x86_64
cargo test brainfuxk --features x86_64 --release -- --nocapture # this is fun. just try it.
cargo test           --features aarch64                         # build for aarch64. a few features are implemented.
cargo test           --features riscv64                         # currently doesn't work. need help.
```

# Example

- [Generate a function which calculates a fibonacci number](./tests/demo.rs)

```sh
cargo test demo --features $ARCH -- --nocapture # $ARCH is x86_64 or aarch64
```

- Useful macro is available to describe IR

```rust
let fibo = sericum_ir!(m; define [i32] f [(i32)] {
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

# Make your own language using sericum as backend

``./minilang`` and ``./sericumcc`` may help you.
