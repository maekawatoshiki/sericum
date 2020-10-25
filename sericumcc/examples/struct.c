#include "assert.h"

int main() {
  typedef struct {
    int x, y, z;
  } A;
  A a[1];
  A c[2][2];
  a[0].y=1;
  A b;
  b.x=3;
  c[1][1].z=4;
  c[0][1].y=1;
  assert(b.x+a[0].y+c[1][1].z+c[0][1].y == 9);
  return 0;
}
