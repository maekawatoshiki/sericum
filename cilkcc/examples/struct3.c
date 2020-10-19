#include "assert.h"

typedef struct  {
  int x, y, z;
} A;

int test(A a[3]) {
  assert(a[1].y == 2);
  return 0;
}

int main() {
  A a[3];
  a[0].x = 1;
  a[1].y = 2;
  assert(a[0].x == 1);
  assert(a[1].y == 2);
  test(a);
  return 0;
}
