// show mandelbrot set.

double pow(double, double);

int mandelbrot(double c_x, double c_y, int n) {
  double x_n = 0.0, y_n = 0.0,
         x_n_1, y_n_1;
  for(int i = 0; i < n; i+=1) {
    x_n_1 = pow(x_n, 2.0) - pow(y_n, 2.0) + c_x;
    y_n_1 = 2.0 * x_n * y_n + c_y;
    if(pow(x_n_1, 2.0) + pow(y_n_1, 2.0) > 4.0) {
      return n;
    } else {
      x_n = x_n_1;
      y_n = y_n_1;
    }
  }
  return 0;
}

int putchar(char);
int puts(char *);

int main() {
  double x_max = 1.0, x_min = 0.0-2.0,
         y_max = 1.0, y_min = 0.0-1.0,
         dx = 0.05, dy = 0.05;
  for(double y = y_max; y > y_min; y -= dy) {
    for(double x = x_min; x < x_max; x += dx) 
      putchar(mandelbrot(x, y, 300) == 0 ? '*' : ' ');
    puts("");
  }
  return 0;
}
