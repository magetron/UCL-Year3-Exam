#include <stdio.h>
#include <unistd.h>

int main () {
  setvbuf(stdout, NULL, _IONBF, 0);
  printf("Hello");
  //fflush(stdout);
  if (fork() == 0) {
    printf("child goodbye\n");
  } else {
    printf("parent goodbye\n");
  }
  return 0;
}
