#!/bin/bash
cd minilang
cargo test
cd ..
cargo test
REPORT=$(find ./target/debug -maxdepth 2 -regex '.+/deps/.*' -a ! -regex '.+\.\(d\|rlib\|rmeta\|so\)')
for file in $REPORT; do  
  echo $file
  kcov --include-pattern=cilk/src --exclude-pattern=/.cargo ./target/cov "$file"
done
bash <(curl -s https://codecov.io/bash) -s ./target/cov -t $1
