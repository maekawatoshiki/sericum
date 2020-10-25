#include "assert.h"

int main() {
  int i, sum;
  i = 0; sum = 0;
  while (i <= 10) {
    sum += i;
    i += 1;
  }
  assert(sum == 55);
  return 0;
}
