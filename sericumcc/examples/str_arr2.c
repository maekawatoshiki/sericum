#include "examples/assert.h"

int strcmp(char *, char *);

int main() {
  char *s[2][2] = { {"the", "quick"}, {"brown", "fox"} };
  assert(strcmp(s[0][0], "the"   ) == 0);
  assert(strcmp(s[0][1], "quick" ) == 0);
  assert(strcmp(s[1][0], "brown" ) == 0);
  assert(strcmp(s[1][1], "fox"   ) == 0);
  return 0;
}
