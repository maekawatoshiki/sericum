#include <sample.h>
#define master(x, y) main(int x, char ** y)
#define A 10
#define B 20
#if defined (A) + defined B == 2
  #define C 0
#else 
  #define C 100
#endif

int master(argc, argv) {
  return C;
}
