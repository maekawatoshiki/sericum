#include "assert.h"

int main() {
  int a[10];
  a[1] = 2;
  a[3] = 5;
  assert(a[1] + a[3] == 7);
  return 0;
}
