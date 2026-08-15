// Derived from coremark/barebones/core_portme.c.
//
// Copyright 2018 Embedded Microprocessor Benchmark Consortium (EEMBC),
// licensed under the Apache License 2.0.  See ../../vendor/coremark/LICENSE.md.
//
// Upstream leaves barebones_clock() and portable_init() as #error stubs for the
// port to fill in.  Time is the mcycle CSR, and there is no board to bring up.

#include "coremark.h"
#include "core_portme.h"

#if VALIDATION_RUN
volatile ee_s32 seed1_volatile = 0x3415;
volatile ee_s32 seed2_volatile = 0x3415;
volatile ee_s32 seed3_volatile = 0x66;
#endif
#if PERFORMANCE_RUN
volatile ee_s32 seed1_volatile = 0x0;
volatile ee_s32 seed2_volatile = 0x0;
volatile ee_s32 seed3_volatile = 0x66;
#endif
#if PROFILE_RUN
volatile ee_s32 seed1_volatile = 0x8;
volatile ee_s32 seed2_volatile = 0x8;
volatile ee_s32 seed3_volatile = 0x8;
#endif
volatile ee_s32 seed4_volatile = ITERATIONS;
volatile ee_s32 seed5_volatile = 0;

CORETIMETYPE
barebones_clock(void)
{
    CORETIMETYPE cycles;
    __asm__ volatile("csrr %0, mcycle" : "=r"(cycles));
    return cycles;
}

#define GETMYTIME(_t)        (*_t = barebones_clock())
#define MYTIMEDIFF(fin, ini) ((fin) - (ini))
// mcycle is the finest resolution there is, and a 64b tick cannot overflow.
#define TIMER_RES_DIVIDER          1
#define SAMPLE_TIME_IMPLEMENTATION 1
// The driver simulates the System domain, whose period is 10ns.  Nothing prints
// seconds, so this only has to be self-consistent.
#define EE_TICKS_PER_SEC (100000000 / TIMER_RES_DIVIDER)

static CORETIMETYPE start_time_val, stop_time_val;

void
start_time(void)
{
    GETMYTIME(&start_time_val);
}

void
stop_time(void)
{
    GETMYTIME(&stop_time_val);
}

CORE_TICKS
get_time(void)
{
    return (CORE_TICKS)(MYTIMEDIFF(stop_time_val, start_time_val));
}

secs_ret
time_in_secs(CORE_TICKS ticks)
{
    return ((secs_ret)ticks) / (secs_ret)EE_TICKS_PER_SEC;
}

ee_u32 default_num_contexts = 1;

void
portable_init(core_portable *p, int *argc, char *argv[])
{
    (void)argc;
    (void)argv;
    p->portable_id = 1;
}

void
portable_fini(core_portable *p)
{
    p->portable_id = 0;
}
