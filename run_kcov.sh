#!/bin/bash
cd minilang
RUST_BACKTRACE=full cargo test -- --test-threads=1
cd ..
cd sericumcc
RUST_BACKTRACE=full cargo test 
cd ..
RUST_BACKTRACE=full cargo test --features x86_64 -- --test-threads=1
REPORT=$(find ./target/debug -maxdepth 2 -regex '.+/deps/.*' -a ! -regex '.+\.\(d\|rlib\|rmeta\|so\)')
for file in $REPORT; do  
  echo $file
  kcov --include-pattern=sericum/src --exclude-pattern=/.cargo ./target/cov "$file"
done
bash <(curl -s https://codecov.io/bash) -s ./target/cov -t $1
