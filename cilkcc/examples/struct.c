int main() {
  typedef struct {
    int a;
  } A;
  A a;
  a.a = 1;
  a.a += a.a;
  return a.a;
}
