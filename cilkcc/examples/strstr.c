#include "assert.h"

int find(char *s, char *q) {
  int s_i = 0, q_i = 0;
  while (s[s_i] != 0) {
    if (q[q_i] == 0) return s_i-q_i;
    if (s[s_i] == q[q_i]) q_i += 1;
    else if (q_i > 0) q_i = 0;
    s_i += 1;
  }
  return 0 - 1;
}

int main() {
  char *s = "the quick brown fox jumps over the lazy dog";
  int p = find(s, "brown");
  assert(p == 10);
  return 0;
}
