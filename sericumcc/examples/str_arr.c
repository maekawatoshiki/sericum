#include "assert.h"

int strcmp(char *, char *);

int main() {
  char *s[2] = {"hello", "world"};
  assert(strcmp(s[0], "hello") == 0);
  assert(strcmp(s[1], "world") == 0);
  return 0;
}
