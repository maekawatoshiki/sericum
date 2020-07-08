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

### 動かしてみよう

IRを見るだけでは面白くないかもしれないので，実際に動作させてみましょう．
x86_64向けJITが実装されているのでそれを使います．（コンパイラ基盤なので本来はアセンブリの出力などが主です．RISC-V向けバックエンドもアセンブリ出力しかサポートしていません．）

./src/main.rs を次のように編集しましょう．


```rs
extern crate cilk;
use cilk::ir::{
    builder::{Builder, FunctionIdWithModule},
    module::Module,
    types::Type,
    value::Value,
};
use cilk::codegen::arch::exec::jit::JITExecutor;

fn main() {
    let mut module = Module::new("cilk");
    let func_id = module.create_function("main", Type::Int32, vec![]);
    let mut builder = Builder::new(FunctionIdWithModule::new(&mut module, func_id));
    let entry = builder.append_basic_block();
    builder.set_insert_point(entry);
    builder.build_ret(Value::new_imm_int32(42));

    println!("{:?}", module);

    let mut jit = JITExecutor::new(&mut module);
    let func = jit.find_function_by_name("main").unwrap();
    println!("Result: {:?}", jit.run(func, vec![]));
}
```

そして ``cargo run`` すると，

```
... 省略 ...
Result: Int32(42)
```

と表示されると思います．生成した``main``関数が``42``を返していることがわかります．


## 解説

さて，特にコードについて説明をしていなかったのでここからは一行ずつ見ていきます．

### モジュールを作る

cilkという名前のモジュールを作ります．
モジュールは関数などを束ねておくためのもので，これ抜きでは何も始まりません．

```rs
let mut module = Module::new("cilk");
```

### 関数を作る

``module.create_function`` の第一引数には関数名，第二引数には返却型，第三引数には引数型を指定します．
create_functionは関数(Function)そのものでなく識別ID(FunctionId)を返します．
IDはCopyが実装してあるので，取り回しが楽です．

```rs
let func_id = module.create_function("main", Type::Int32, vec![]);
```

### ビルダーを作る

BuilderはBasic Blockや命令の追加など様々なことを簡単に行えるようにしてくれます．
実はBuilderを使わなくても同じことが行えますが，面倒なので今はやめましょう．

```rs
let mut builder = Builder::new(FunctionIdWithModule::new(&mut module, func_id));
```

勘の鋭い人は ``FunctionIdWithModule`` が何なのか気になったかもしれません．
実は ``FunctionEntity`` というものを指定することもできます．

```rs
let func = module.function_ref_mut(func_id);
let mut builder = Builder::new(FunctionEntity(func));
```

FunctionIdWithModuleは関数IDとモジュールへのミュータブルな参照を必要としますが，
FunctionEntityは関数へのミュータブルな参照のみを必要とします．
Builderを作るときに，モジュールへのミュータブルな参照にアクセス可能なときはFunctionIdWithModuleを使うのが一番楽です．
状況によって使い分けましょう．


### 基本ブロックの作成・追加

``builder.append_basic_block`` で新たな基本ブロックの追加が行えます．
そして，``builder.set_insert_point`` でビルダーは指定した基本ブロックに命令を追加できるようになります．

```rs
let entry = builder.append_basic_block();
builder.set_insert_point(entry);
```


### returnの生成

ビルダーで様々な命令を生成できますが，今回は ``builder.build_ret`` でreturnを生成します．
列挙型``Value``に様々な値の種類が定義してありますが，整数が欲しいので``Value::new_imm_int32``で32bit符号付整数を得ます．

```rs
builder.build_ret(Value::new_imm_int32(42));
```

## ビルダーは面倒?

上記のコードではビルダーを使って命令を生成しました．
LLVMのC++ APIを使ったことある方なら見慣れた感じだと思います．
ですが，``ret i32 42``が欲しいだけなのに面倒だなぁと思う人もいるかもしれません．

cilkではマクロを使ってより簡単にIRを記述することができます．

### cilk_ir!

モジュールの定義以外を消して，以下を追加してみてください．
今まで通りのIRが生成されます．


```rs
use cilk::*;
cilk_ir!(module; define [i32] main [] {
    entry:
        ret (i32 42);
});
```

とてもわかりやすいですよね？

詳しい文法は別の機会に解説しますが (cilk/src/macros.rsを読むのも良いでしょう．だれかproc-macroに書き直してくれる方を募集しています...)
今後はこの便利なマクロを使ってIRを記述していきます．
まだ融通の効かないところも多いですが... (``use cilk::*``で名前空間を汚染してしまうとか)

## 次のページ

次のページは，[変数](./2-変数.md)です．
いよいよプログラムらしいプログラムを生成していきます．
