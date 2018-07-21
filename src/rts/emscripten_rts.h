#ifndef CMM_OF_WASM_EMSCRIPTEN_RTS
#define CMM_OF_WASM_EMSCRIPTEN_RTS

#include <sys/time.h>
#include <sys/uio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "wasm-rt.h"

#define WASM_PAGE_SIZE 65536

uint64_t env_global_ABORT;
uint64_t env_global_EXITSTATUS;
double global_global_NaN;
double global_global_Infinity;

// Hack: just taking this from PolyBenchC output code at the moment
uint64_t env_global_TOTAL_STACK;
uint64_t env_global_TOTAL_MEMORY;

uint64_t env_global_GLOBAL_BASE;

uint64_t env_global_STATIC_BASE;
uint64_t env_global_STATIC_BUMP;
uint64_t env_global_STATICTOP;
uint64_t env_global_STACK_BASE;
uint64_t env_global_STACKTOP;
uint64_t env_global_STACK_MAX;
uint64_t env_global_DYNAMIC_BASE;
uint64_t env_global_DYNAMICTOP_PTR;
uint64_t env_global_tempDoublePtr;

uint64_t env_global_tableBase;
uint64_t env_global_memoryBase;

wasm_rt_table_t env_table_table;
wasm_rt_memory_t env_memory_memory;

void env_cfunc_abort(char* str);
uint32_t env_cfunc____syscall54(uint32_t which, uint32_t varargs);
uint32_t env_cfunc____syscall6(uint32_t which, uint32_t varargs);
uint32_t env_cfunc____syscall140(uint32_t which, uint32_t varargs);
uint32_t env_cfunc____syscall146(uint32_t which, uint32_t varargs);
uint32_t env_cfunc__emscripten_memcpy_big(uint32_t dest,
    uint32_t src, uint32_t num);

void env_cfunc_nullFunc_ii(char* x);
uint32_t env_cfunc____setErrNo(uint32_t value);
void env_cfunc____assert_fail(char* condition, char* filename,
    int line, void* func);
uint32_t env_cfunc__gettimeofday(uint32_t ptr);
void env_cfunc____lock();
void env_cfunc____unlock();
void env_cfunc_abortOnCannotGrowMemory();
void env_cfunc_enlargeMemory();
uint32_t env_cfunc_getTotalMemory();
void env_cfunc_abortStackOverflow(uint32_t alloc_size);
void env_cfunc___exit(uint32_t status);
void env_cfunc__exit(uint32_t status);

void env_init();

#endif
