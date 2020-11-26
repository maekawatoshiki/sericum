#include "assert.h"

typedef struct {
  int x, y;
} Point;

int func(Point *p) {
  assert(p->x == 1);
  assert(p->y == 1);
  p->x += 1;
  return 0;
}

int func2(int a, Point p, int b, Point p2, int x, int y, int z) {
  assert(a == 12345678);
  assert(p.x == 2);
  assert(p.y == 1);
  assert(b == 321);
  assert(p2.x == 2);
  assert(p2.y == 3);
  assert(x == 3);
  assert(y == 4);
  assert(z == 5);
  return 0;
}

int main() {
  Point p;
  p.x = 1; p.y = 1;
  Point p2;
  p2.x = 2; p2.y = 3;

  assert(p.x == 1);
  func(&p);
  func2(12345678, p, 321, p2, 3, 4, 5);
  assert(p.y == 1);
  return 0;
}
