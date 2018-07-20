#include "emscripten_rts.h"

void err(char** str) {
  puts(str);
}

void abort(char** str) {
  env_global_ABORT = 1;
  env_global_EXITSTATUS = 1;
  printf("abort: %s", str);
}

// I guess these were needed for pthreads support?
// Nothing in our output anyway.
void env_cfunc____lock() {
}

void env_cfunc____unlock() {
}

void env_cfunc_abortOnCannotGrowMemory() {
  abort("Memory is static for now.");
}

void env_cfunc_enlargeMemory() {
  env_cfunc_abortOnCannotGrowMemory();
}

int env_cfunc____setErrNo(int value) {
  // FIXME: This should call XXX_cfunc____errno_location() to
  // ascertain the error number location, and then write the
  // error number to the result >>2 in the heap.
  return value;
}

uint64_t env_cfunc_getTotalMemory() {
  return env_global_TOTAL_MEMORY;
}

void env_cfunc_abortStackOverflow(alloc_size) {
  char buffer[100];
  // TODO: It would be nice to print out the amount the stack overflowed
  // by, but for this, we need "stackSave" which we would have to link...
  sprintf(buffer, "Stack overflow! Attempted to allocate %d bytes on the stack.");
}

void env_init() {
  // Initialise stack top / max
  env_global_STACKTOP =
    env_global_STACK_BASE + env_global_TOTAL_STACK;
  env_global_STACK_MAX = env_global_STACK_BASE;
  env_global_STATIC_BASE = env_global_GLOBAL_BASE;
  env_global_STATICTOP = STATIC_BASE + env_global_STATIC_BUMP;
  env_global_tempDoublePtr = env_global_STATICTOP;
  env_global_STATICTOP += 16;

  // Allocate table and memory
  wasm_rt_allocate_memory(&env_memory_memory, env_global_TOTAL_MEMORY / WASM_PAGE_SIZE, env_global_TOTAL_MEMORY / WASM_PAGE_SIZE);
  wasm_rt_allocate_table(&env_table_table, 1024, -1);

}
