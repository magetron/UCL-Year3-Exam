#include <stdio.h>

int main () {
  unsigned long a = 100;
  unsigned long b = 200;
  unsigned long x = 50;
  unsigned long r = 0;

  r = (- (unsigned long) (a < b)) & x;
  printf("%lu\n", r);
  return 0;
}
