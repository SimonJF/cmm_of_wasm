#include <stdio.h>
#include <stdint.h>
#include "libif.h"

int main() {

  printf("nested 0 0: %i\n", nested(0, 0));
  printf("nested 1 0: %i\n", nested(1, 0));
  printf("nested 0 1: %i\n", nested(0, 1));
  printf("nested 3 2: %i\n", nested(3, 2));
  printf("nested 0 -100: %i\n", nested(0, -100));
  printf("nested 10 10: %i\n", nested(10, 10));
  printf("nested 0 -1: %i\n", nested(0, -1));
  printf("nested -111 -2: %i\n", nested(-111, -2));
}
