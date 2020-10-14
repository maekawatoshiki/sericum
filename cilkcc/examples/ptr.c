#include "assert.h"

int main() {
  int a;
  int *b;
  b = &a;
  a = 123;
  assert(*b == 123);
  *b = 23;
  assert(a == 23);
  return 0;
}
