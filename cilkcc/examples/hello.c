#include <sample.h>
#define master(x, y) main(int x, char ** y)
#define A 10
#define B 20
#if defined (A) + defined B == 2
  #define C 0
#else 
  #define C 100
#endif

int main() {
  int x = 1;

  if (x == 0) { }

  while (x < 2) {  }
  return 0;
}
