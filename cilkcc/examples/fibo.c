#include "assert.h"

int fibo(int x) {
  if (x <= 2) return 1;
  return fibo(x - 1) + fibo(x - 2);
}

int main() {
  assert(fibo(10) == 55);
  return 0;
}
