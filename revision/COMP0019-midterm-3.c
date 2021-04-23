#include<stdio.h>

struct bar {
  char arr[5];
};

struct foo {
  struct foo *p;
  struct bar x[3];
  struct bar *q;
};

int main () {
  printf("%lu %lu %lu\n", sizeof(struct bar), sizeof(struct bar *), sizeof(struct foo));

  struct foo* f = NULL;
  printf("%lu\n", sizeof(*f));

  return 0;
}
