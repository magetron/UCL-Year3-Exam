#include <stdint.h>

#define PAGEOFFBITS     12                   // # bits in page offset
#define PAGESIZE        (1 << PAGEOFFBITS)   // Size of page in bytes
#define PAGEINDEXBITS   9                    // # bits in a page index level
#define NPAGETABLEENTRIES (1 << PAGEINDEXBITS) // # entries in page table page
typedef uint64_t x86_64_pageentry_t;
typedef struct __attribute__((aligned(PAGESIZE))) x86_64_pagetable {
      x86_64_pageentry_t entry[NPAGETABLEENTRIES];
} x86_64_pagetable;

#include <stdio.h>

int main () {
  printf("%lu\n", sizeof(struct x86_64_pagetable));
  return 0;
}
