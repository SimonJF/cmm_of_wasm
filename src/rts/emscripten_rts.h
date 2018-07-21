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

#define WASM_PAGE_SIZE 65535

uint64_t env_global_ABORT = 0;
uint64_t env_global_EXITSTATUS = 0;
double env_global_NaN = NAN;
double env_global_Infinity = INFINITY;

// Hack: just taking this from PolyBenchC output code at the moment
uint64_t env_global_TOTAL_STACK = 5242880;
uint64_t env_global_TOTAL_MEMORY = 16777216;

uint64_t env_global_GLOBAL_BASE = 1024;

uint64_t env_global_STATIC_BASE = 0;
uint64_t env_global_STATIC_BUMP = 5840;
uint64_t env_global_STATICTOP = 0;
uint64_t env_global_STACK_BASE = 0;
uint64_t env_global_STACKTOP = 0;
uint64_t env_global_STACK_MAX = 0;
uint64_t env_global_DYNAMIC_BASE = 0;
uint64_t env_global_DYNAMICTOP_PTR = 0;
uint64_t env_global_tempDoublePtr = 0;

uint64_t env_global_tableBase = 0;
uint64_t env_global_memoryBase = 0;

wasm_rt_table_t env_table_table;
wasm_rt_memory_t env_memory_memory;


uint32_t env_cfunc____syscall54(uint32_t which, uint32_t varargs);
uint32_t env_cfunc____syscall6(uint32_t which, uint32_t varargs);
uint32_t env_cfunc____syscall140(uint32_t which, uint32_t varargs);
uint32_t env_cfunc____syscall146(uint32_t which, uint32_t varargs);
uint32_t env_cfunc__emscripten_memcpy_big(uint32_t dest,
    uint32_t src, uint32_t num);

#endif
