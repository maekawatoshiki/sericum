int f(double a, double b) {
  if (a < b) return 0;
  return 1;
}

int main() {
  printf("%d\n", f(1.2, 2.4));
  printf("%d\n", f(3.2, 2.4));
}
