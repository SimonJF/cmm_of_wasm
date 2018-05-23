#include <stdio.h>
#include "libadd.h"


// asm volatile ("call add" : "=a" (result) : "a" (arg1), "b" (arg2) : "memory", "cc");

int main() {
  printf("Simple_add result: %llu\n", simple_add());
  unsigned long long add_res;
  unsigned long long arg1 = 10;
  unsigned long long arg2 = 15;
  asm volatile ("call add" : "=a" (add_res) : "a" (arg1), "b" (arg2) : "memory", "cc");
  printf("add 10, 15 result: %llu\n", add_res);
  //printf("add 1000, 20000 result: %llu\n", add(1000, 20000));
}
