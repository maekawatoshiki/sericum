# まずはreturnから

ここでは，``42``という数値 (32bit整数) を返却する ``main`` 関数を作ります．

## プロジェクトを作る

### 必要なもの

- Nightly Rust 
- Cargo

(まだ導入していない場合は，rustupでまとめて手に入れましょう．)

### cilkをクローンする

cilkは[crates.io](https://crates.io)に登録してありますが，それでは古いので手動でGitHubからクローンしましょう．

```sh
git clone https://github.com/maekawatoshiki/cilk
```

### Cargoでプロジェクト作成

これからcilkを使ったコードを書いていくために，プロジェクトを作成します．``cargo`` が大体なんでもやってくれます．

```sh
cargo new try_cilk --bin
cd try_cilk
```

### Cargo.tomlを編集する

Cargo.toml の dependencies に cilk を追加します．

```Cargo.toml:toml
... 省略 ...
[dependencies]
cilk = { path = "../cilk", features = ["x86_64"] }
```

features に x86_64 が指定してありますが，riscv64 を指定することもできます．
その場合RISC-V向けのコード生成が可能になりますが，まだまだ発展途上のバックエンドなので使用はあまりおすすめしません．
もし使いたい場合は ./cilk/tests 以下のテストを読むと良いです．プルリクも歓迎です．

### ./src/main.rsを編集する

先頭に ``extern crate cilk;``を追加します．

一度ここでコンパイルしてみましょう．``cargo run`` でハローワールドが表示されれば成功です．まだcilkの機能は全く使っていません．

## IRを生成する

それではいよいよ，実際にIRを生成していきます．

### IRの構造

ちょっとその前に一言だけ．cilkのIRはLLVM IRに大きく影響を受けており，とても似通っています．
Module > Function > Basic Block > Instruction という階層構造も同じです．APIもなるべく似せるようにしています．

### IRを見てみよう

まずは ./src/main.rs に書かれているものを消して下さい．そして以下を追加．

```rs
extern crate cilk;
use cilk::ir::{
    builder::{Builder, FunctionIdWithModule},
    module::Module,
    types::Type,
    value::Value,
};

fn main() {
    let mut module = Module::new("cilk");
    let func_id = module.create_function("main", Type::Int32, vec![]);
    let mut builder = Builder::new(FunctionIdWithModule::new(&mut module, func_id));
    let entry = builder.append_basic_block();
    builder.set_insert_point(entry);
    builder.build_ret(Value::new_imm_int32(42));

    println!("{:?}", module);
}
```

そして実行

```sh
cargo run 
```

以下のようなIRが表示されたら成功です!

```c
Module (name: cilk)
define i32 main() {
label.0:	// pred(), succ(), def(), in(), out()
    ret i32 42
}
```
