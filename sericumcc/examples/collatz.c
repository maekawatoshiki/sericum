#include "assert.h"

int printf(char *, int);

int collatz(int x) {
  printf("%d\n", x);
  if (x == 1) return 1;
  if (x % 2 == 0) return collatz(x / 2);
  if (x % 2 == 1) return collatz(3 * x + 1);
  return 0;
}

int main() {
  assert(collatz(123) == 1);
  return 0;
}
