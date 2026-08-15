// Derived from riscv-tests/env/p/riscv_test.h.
//
// Copyright (c) 2012-2015, The Regents of the University of California (Regents).
// All Rights Reserved.  See ../LICENSE for license details.
//
// The upstream environment initialises PMP, address translation and trap
// delegation, and reports its result by storing to the `tohost` MMIO word.
// cuintet implements none of that: its CSR file is mtvec, mepc and mcause.
//
// So this environment keeps only what cuintet can execute.  A test leaves its
// result in TESTNUM and executes ECALL; the testbench halts at that ECALL and
// reads the register, so no trap handler and no MMIO are involved.

#ifndef _ENV_CUINTET_H
#define _ENV_CUINTET_H

#define TESTNUM gp

//-----------------------------------------------------------------------
// Begin Macro
//-----------------------------------------------------------------------

// `init` is the per-extension hook the test bodies expand before their first
// instruction.  RV32I needs nothing enabled, so it is empty.
#define RVTEST_RV32U                                                    \
  .macro init;                                                          \
  .endm

#define RVTEST_RV64U RVTEST_RV32U

#define INIT_XREG                                                       \
  li x1, 0;  li x2, 0;  li x3, 0;  li x4, 0;                            \
  li x5, 0;  li x6, 0;  li x7, 0;  li x8, 0;                            \
  li x9, 0;  li x10, 0; li x11, 0; li x12, 0;                           \
  li x13, 0; li x14, 0; li x15, 0; li x16, 0;                           \
  li x17, 0; li x18, 0; li x19, 0; li x20, 0;                           \
  li x21, 0; li x22, 0; li x23, 0; li x24, 0;                           \
  li x25, 0; li x26, 0; li x27, 0; li x28, 0;                           \
  li x29, 0; li x30, 0; li x31, 0;

// The testbench halts at the ECALL itself, so control never reaches mtvec on
// the pass or fail path.  Anything that does arrive is an unexpected trap:
// spin, and let the cycle budget report it as a timeout.
#define RVTEST_CODE_BEGIN                                               \
        .section .text.init;                                            \
        .align 2;                                                       \
        .globl _start;                                                  \
_start:                                                                 \
        j reset_vector;                                                 \
        .align 2;                                                       \
trap_vector:                                                            \
        j trap_vector;                                                  \
reset_vector:                                                           \
        INIT_XREG;                                                      \
        li TESTNUM, 0;                                                  \
        la t0, trap_vector;                                             \
        csrw mtvec, t0;                                                 \
        init;

//-----------------------------------------------------------------------
// End Macro
//-----------------------------------------------------------------------

// Upstream traps here with `unimp`, which cuintet does not decode.  Spinning
// reports a fall-through as a timeout instead of a decode error.
#define RVTEST_CODE_END                                                 \
        j .

//-----------------------------------------------------------------------
// Pass/Fail Macro
//-----------------------------------------------------------------------

// TESTNUM == 1 is a pass; anything else is `failing testnum << 1 | 1`.  The
// a7/a0 syscall ABI is dropped, since it only mattered to the HTIF proxy
// behind `tohost`.  The `fence` stays: it costs nothing and makes every test
// in the suite exercise the MISC-MEM decode.
#define RVTEST_PASS                                                     \
        fence;                                                          \
        li TESTNUM, 1;                                                  \
        ecall

// A zero TESTNUM would encode as a pass, so refuse to report it.
#define RVTEST_FAIL                                                     \
        fence;                                                          \
1:      beqz TESTNUM, 1b;                                               \
        sll TESTNUM, TESTNUM, 1;                                        \
        or TESTNUM, TESTNUM, 1;                                         \
        ecall

//-----------------------------------------------------------------------
// Data Section Macro
//-----------------------------------------------------------------------

#define RVTEST_DATA_BEGIN .align 4; .global begin_signature; begin_signature:
#define RVTEST_DATA_END .align 4; .global end_signature; end_signature:

#endif
