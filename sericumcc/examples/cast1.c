#include "assert.h"

int main() {
  int a = 123;
  int *b = &a;
  char *c = (char *)b;
  int *d = (int *)c;
  *d = 23;
  assert(a == 23);
  return 0;
}
