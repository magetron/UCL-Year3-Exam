#include <stdio.h>

struct symstring {
  char *s;
  unsigned short len;
};

int main () {
  printf("%lu\n", sizeof(struct symstring));
  return 0;
}
