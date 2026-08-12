// Derived from coremark/barebones/core_portme.h.
//
// Copyright 2018 Embedded Microprocessor Benchmark Consortium (EEMBC),
// licensed under the Apache License 2.0.  See ../../vendor/coremark/LICENSE.md.
//
// cuintet has no OS, no libc and no I/O.  So time is the mcycle CSR, the work
// area comes off the stack, and ee_printf drops everything it is given: the
// result of a run is that crt0.S reaches its ECALL at all.

#ifndef CORE_PORTME_H
#define CORE_PORTME_H

#include <stddef.h>

#define HAS_FLOAT  0
#define HAS_TIME_H 0
#define USE_CLOCK  0
#define HAS_STDIO  0
#define HAS_PRINTF 0

// main() takes no arguments here, and returns to crt0.S as usual.
#define MAIN_HAS_NOARGC   1
#define MAIN_HAS_NORETURN 0

#define COMPILER_VERSION "GCC" __VERSION__
#define COMPILER_FLAGS   FLAGS_STR
#define MEM_LOCATION     "STACK"

typedef signed short   ee_s16;
typedef unsigned short ee_u16;
typedef signed int     ee_s32;
typedef double         ee_f32;
typedef unsigned char  ee_u8;
typedef unsigned int   ee_u32;
// barebones has ee_u32 here, which truncates an RV64 pointer and so breaks
// align_mem below.
typedef unsigned long ee_ptr_int;
typedef size_t        ee_size_t;

// Aligns an offset up to the next 32b boundary.
#define align_mem(x) (void *)(4 + (((ee_ptr_int)(x) - 1) & ~3))

// mcycle is 64b wide; keeping the tick type that wide costs nothing.
#define CORETIMETYPE unsigned long
typedef unsigned long CORE_TICKS;

#define SEED_METHOD SEED_VOLATILE
#define MEM_METHOD  MEM_STACK

#define MULTITHREAD 1
#define USE_PTHREAD 0
#define USE_FORK    0
#define USE_SOCKET  0

extern ee_u32 default_num_contexts;

typedef struct CORE_PORTABLE_S
{
    ee_u8 portable_id;
} core_portable;

void portable_init(core_portable *p, int *argc, char *argv[]);
void portable_fini(core_portable *p);

// TOTAL_DATA_SIZE defaults to 2000, which selects the performance run.
#if !defined(PROFILE_RUN) && !defined(PERFORMANCE_RUN) && !defined(VALIDATION_RUN)
#if (TOTAL_DATA_SIZE == 1200)
#define PROFILE_RUN 1
#elif (TOTAL_DATA_SIZE == 2000)
#define PERFORMANCE_RUN 1
#else
#define VALIDATION_RUN 1
#endif
#endif

int ee_printf(const char *fmt, ...);

#endif /* CORE_PORTME_H */
