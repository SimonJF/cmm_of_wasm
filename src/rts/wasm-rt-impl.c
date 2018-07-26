/*
 * Copyright 2018 WebAssembly Community Group participants
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#define _GNU_SOURCE
#include "wasm-rt-impl.h"
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <signal.h>
#include <unistd.h>
#include <err.h>

#define PAGE_SIZE 65536

uint32_t wasm_rt_call_stack_depth;

jmp_buf g_jmp_buf;

void wasm_rt_trap(wasm_rt_trap_t code) {
  assert(code != WASM_RT_TRAP_NONE);
  longjmp(g_jmp_buf, code);
}


// Handles segmentation faults when using mmap'ed memory,
// allowing memory violations to be reported gracefully
// as traps
void wasm_rt_signal_handler(int sig, siginfo_t* info, void* something_else) {
  if (sig == SIGSEGV) {
    longjmp(g_jmp_buf, WASM_RT_TRAP_OOB);
  } else if (sig == SIGFPE) {
    // TODO: This could also be int overflow; consider collapsing
    // into a single type of trap
    longjmp(g_jmp_buf, WASM_RT_TRAP_DIV_BY_ZERO);
  }
}

void wasm_rt_setup_signal_handlers() {
  struct sigaction sa;
  memset(&sa, 0, sizeof(struct sigaction));
  sigemptyset(&(sa.sa_mask));

  sa.sa_flags = SA_NODEFER;
  sa.sa_sigaction = wasm_rt_signal_handler;

  sigaction(SIGSEGV, &sa, NULL);
  sigaction(SIGFPE, &sa, NULL);
}

void wasm_rt_malloc_memory(wasm_rt_memory_t* memory) {
  memory->data = calloc(memory->size, 1);
}

// mmap allows us to allocate an area of memory such that we are
// guaranteed to raise a segmentation fault should the access
// exceed the memory bounds.
void wasm_rt_mmap_memory(wasm_rt_memory_t* memory) {
  // Reserve (but don't allocate!) 8GB.
  uint64_t size = 0x00000001FFFFFFFF;

  uint8_t* ptr =
    (uint8_t*)(mmap(NULL, size, PROT_NONE,
          MAP_PRIVATE|MAP_ANONYMOUS, -1, 0));

  if (ptr == MAP_FAILED) {
    err(1, "mmap");
    exit(1);
  }

  // Mark the accessible pages as read/write enabled
  if (mprotect(ptr, memory->size, PROT_READ|PROT_WRITE) == -1) {
    err(1, "mprotect");
    exit(1);
  }

  memory->data = ptr;
}

void wasm_rt_allocate_memory(wasm_rt_memory_t* memory,
                           uint32_t initial_pages,
                           uint32_t max_pages,
                           bool use_mmap) {
  memory->pages = initial_pages;
  memory->max_pages = max_pages;
  uint32_t size = initial_pages * PAGE_SIZE;
  memory->size = size;
  if (use_mmap) {
    wasm_rt_mmap_memory(memory);
    memory->use_mmap = true;
  } else {
    wasm_rt_malloc_memory(memory);
    memory->use_mmap = false;
  }
}


uint32_t wasm_rt_grow_memory(wasm_rt_memory_t* memory, uint32_t delta) {
  uint32_t old_pages = memory->pages;
  uint32_t new_pages = memory->pages + delta;
  // Check whether we can grow
  if (new_pages < old_pages || new_pages > memory->max_pages) {
    return (uint32_t) -1;
  }

  if (new_pages == 0) {
    return (uint32_t) 0;
  }

  // If so, set new pages and new size
  uint32_t old_size = memory->size;
  uint32_t new_size = new_pages * PAGE_SIZE;
  memory->size = new_size;
  memory->pages = new_pages;

  // If we're using mmap'ed memory, mremap, otherwise realloc
  if (memory->use_mmap) {
    mprotect(memory->data, new_size, PROT_READ|PROT_WRITE);
  } else {
    memory->data = realloc(memory->data, new_size);
  }

  return old_pages;
}


void wasm_rt_allocate_table(wasm_rt_table_t* table,
                            uint32_t elements,
                            uint32_t max_elements) {
  table->size = elements;
  table->max_size = max_elements;
  table->data = calloc(table->size, sizeof(wasm_rt_elem_t));
}


/* Primitives not provided by CMM that we're implementing as part of the
 * RTS. Hopefully this section will shrink with time. */

u32 wasm_rt_popcount_u32(u32 i) {
  return __builtin_popcount(i);
}

u64 wasm_rt_popcount_u64(u64 i) {
  return __builtin_popcountll(i);
}

u32 wasm_rt_clz_u32(u32 i) {
  if (i == 0) {
    return 32;
  }
  return __builtin_clz(i);
}

u64 wasm_rt_clz_u64(u64 i) {
  if (i == 0) {
    return 64;
  }
  return __builtin_clzll(i);
}

u32 wasm_rt_ctz_u32(u32 i) {
  if (i == 0) {
    return 32;
  }
  return __builtin_ctz(i);
}

u64 wasm_rt_ctz_u64(u64 i) {
  if (i == 0) {
    return 64;
  }
  return __builtin_ctzll(i);
}

/* This is *hideous*. */
f32 wasm_rt_zero_min_f32(f32 f1, f32 f2) {
  if (signbit(f1)) {
    return f1;
  }
  return f2;
}

f32 wasm_rt_zero_max_f32(f32 f1, f32 f2) {
  if (signbit(f1)) {
    return f2;
  }
  return f1;
}

f64 wasm_rt_zero_min_f64(f64 f1, f64 f2) {
  if (signbit(f1)) {
    return f1;
  }
  return f2;
}

f64 wasm_rt_zero_max_f64(f64 f1, f64 f2) {
  if (signbit(f1)) {
    return f2;
  }
  return f1;
}

f32 wasm_rt_load_f32(wasm_rt_memory_t* mem, u64 offset) {
  f32 result;
  memcpy(&result, mem->data + offset, sizeof(result));
  return result;
}

void wasm_rt_store_f32(wasm_rt_memory_t* mem, u64 offset, f32 to_store) {
  memcpy(mem->data + offset, &to_store, sizeof(f32));
}

f32 wasm_rt_get_global_f32(f32* ptr) {
  f32 result;
  memcpy(&result, ptr, sizeof(result));
  return result;
}

void wasm_rt_set_global_f32(f32* ptr, f32 to_set) {
  memcpy(ptr, &to_set, sizeof(f32));
}

// TODO: Macros?
u32 wasm_rt_reinterpret_u32(f32 bits) {
  u32 result;
  memcpy(&result, &bits, sizeof(result));
  return result;
}

u64 wasm_rt_reinterpret_u64(f64 bits) {
  u64 result;
  memcpy(&result, &bits, sizeof(result));
  return result;
}

f32 wasm_rt_reinterpret_f32(u32 bits) {
  f32 result;
  memcpy(&result, &bits, sizeof(result));
  return result;
}


f64 wasm_rt_reinterpret_f64(u64 bits) {
  f64 result;
  memcpy(&result, &bits, sizeof(result));
  return result;
}

// TODO: MACROS!
u32 wasm_rt_trunc_u32_f32(f32 f) {
  if (f != f) { // NaN
    wasm_rt_trap(WASM_RT_TRAP_INVALID_CONVERSION);
  }
  if (f <= ((f32) -1) || f >= ((f32) UINT32_MAX)) {
    wasm_rt_trap(WASM_RT_TRAP_INT_OVERFLOW);
  }
  return ((u32) f);
}

u32 wasm_rt_trunc_u32_f64(f64 f) {
  if (f != f) {
    wasm_rt_trap(WASM_RT_TRAP_INVALID_CONVERSION);
  }
  if (f <= ((f64) -1) || f > ((f64) UINT32_MAX)) {
    wasm_rt_trap(WASM_RT_TRAP_INT_OVERFLOW);
  }
  return ((u64) f);
}

u64 wasm_rt_trunc_u64_f32(f32 f) {
  if (f != f) { // NaN
    wasm_rt_trap(WASM_RT_TRAP_INVALID_CONVERSION);
  }
  if (f <= ((f32) -1) || f >= ((f32) UINT64_MAX)) {
    wasm_rt_trap(WASM_RT_TRAP_INT_OVERFLOW);
  }
  return ((u64) f);
}

u64 wasm_rt_trunc_u64_f64(f64 f) {
  if (f != f) {
    wasm_rt_trap(WASM_RT_TRAP_INVALID_CONVERSION);
  }
  if (f <= ((f64) -1) || f >= ((f64) UINT64_MAX)) {
    wasm_rt_trap(WASM_RT_TRAP_INT_OVERFLOW);
  }
  return ((u64) f);
}

f32 wasm_rt_convert_f32_u32(u32 i) {
  return ((f32) i);
}
f32 wasm_rt_convert_f32_u64(u64 i) {
  return ((f32) i);
}
f64 wasm_rt_convert_f64_u32(u32 i) {
  return ((f64) i);
}
f64 wasm_rt_convert_f64_u64(u64 i) {
  return ((f64) i);
}

f32 wasm_rt_neg_f32(f32 f) {
  return (-f);
}
