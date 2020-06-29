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

