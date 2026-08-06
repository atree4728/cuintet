/* Derived from riscv-tests/env/p/link.ld.
 *
 * Copyright (c) 2012-2015, The Regents of the University of California
 * (Regents).  All Rights Reserved.  See ../LICENSE for license details.
 *
 * Upstream pads to 4 KiB between sections to leave room for the `tohost` page.
 * This environment has no tohost, so the sections are packed and the image
 * stays a few KiB -- which is the whole simulated memory.
 *
 * The base stays at 0x80000000: cuintet's `toWordAddr` truncates to
 * WordAddrWidth bits, so both lui/addi absolute addresses and auipc
 * PC-relative ones land on the right word of an image loaded at offset 0.
 */

OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{
  . = 0x80000000;
  .text.init : { *(.text.init) }
  .text      : { *(.text .text.*) }
  . = ALIGN(4);
  .data      : { *(.rodata .rodata.*) *(.data .data.*) *(.sdata .sdata.*) }
  . = ALIGN(4);
  .bss       : { *(.sbss .sbss.*) *(.bss .bss.*) *(COMMON) }
  . = ALIGN(4);
  _end = .;
}
