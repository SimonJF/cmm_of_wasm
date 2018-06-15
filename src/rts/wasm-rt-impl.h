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

#ifndef WASM_RT_IMPL_H_
#define WASM_RT_IMPL_H_

#include <setjmp.h>
#include <stdint.h>

#include "wasm-rt.h"

#ifdef __cplusplus
extern "C" {
#endif

/** A setjmp buffer used for handling traps. */
extern jmp_buf g_jmp_buf;

/** Convenience macro to use before calling a wasm function. On first execution
 * it will return `WASM_RT_TRAP_NONE` (i.e. 0). If the function traps, it will
 * jump back and return the trap that occurred.
 *
 * ```
 *   wasm_rt_trap_t code = wasm_rt_impl_try();
 *   if (code != 0) {
 *     printf("A trap occurred with code: %d\n", code);
 *     ...
 *   }
 *
 *   // Call the potentially-trapping function.
 *   my_wasm_func();
 * ```
 */
#define wasm_rt_impl_try() setjmp(g_jmp_buf)


typedef uint8_t u8;
typedef int8_t s8;
typedef uint16_t u16;
typedef int16_t s16;
typedef uint32_t u32;
typedef int32_t s32;
typedef uint64_t u64;
typedef int64_t s64;
typedef float f32;
typedef double f64;

u32 wasm_rt_popcount_u32(u32 i);
u64 wasm_rt_popcount_u64(u64 i);
u32 wasm_rt_clz_u32(u32 i);
u64 wasm_rt_clz_u64(u64 i);
u32 wasm_rt_ctz_u32(u32 i);
u64 wasm_rt_ctz_u64(u64 i);


#ifdef __cplusplus
}
#endif
#endif // WASM_RT_IMPL_H_
