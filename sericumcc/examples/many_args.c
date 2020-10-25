#include "assert.h"

int f(
    int a0,
    int a1,
    int a2,
    int a3,
    int a4,
    int a5,
    int a6,
    int a7,
    int a8,
    int a9,
    int a10,
    int a11,
    int a12
    ){
  return 
    a0
    +a1
    +a2
    +a3
    +a4
    +a5
    +a6
    +a7
    +a8
    +a9
    +a10
    +a11
    +a12;
}

int main() {
  assert(f(
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,//55
      11,//66
      12,//78
      13//91
      ) == 91);
  return 0;
}
