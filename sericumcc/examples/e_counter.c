#include "assert.h"

int main() {
  char *s = "the quick brown fox jumps over the lazy dog", *p = s;
  int i = 0;
  while (*p != 0) {
    if (*p == 'e') i += 1;
    p += 1;
  }
  assert(i == 3);
  return 0;
}
