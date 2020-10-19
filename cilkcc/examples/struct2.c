#include "assert.h"

int main() {
  typedef struct { 
    int x, y, z;
  } A;
  int i = 1;
  A a[3];
  a[i].x = 1;
  a[i+1].z = 2;
  a[i-1].y = 3;
  assert(a[1].x == 1);
  assert(a[0].y == 3);
  assert(a[2].z == 2);
  return 0;
}
