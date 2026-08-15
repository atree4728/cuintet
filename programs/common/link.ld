/* Link a bare-metal benchmark into cuintet's single flat RAM.
 *
 * The base stays at 0x80000000: `toRamAddr` truncates an address to the RAM's
 * width, so an image loaded at word zero is addressed correctly by both
 * lui/addi absolute addresses and auipc PC-relative ones.
 *
 * RAM_SIZE must match the ramAddrWidth the suite is registered with: 8 bytes
 * per word, so 128K == 2^14 words == ramAddrWidth 14.  A benchmark that needs
 * a different size passes -Wl,--defsym=__ram_size=<bytes>.
 */

OUTPUT_ARCH( "riscv" )
ENTRY(_start)

RAM_SIZE = DEFINED(__ram_size) ? __ram_size : 128K;

SECTIONS
{
  . = 0x80000000;
  .text.init : { *(.text.init) }
  .text      : { *(.text .text.*) }
  /* NOPs for the pipeline to fetch into past the end of .text; see crt0.S. */
  .guard     : { *(.guard) }
  . = ALIGN(8);
  .rodata    : { *(.rodata .rodata.*) }
  . = ALIGN(8);
  .data      : { *(.data .data.*) }
  . = ALIGN(8);
  /* gp addresses the 4 KiB around it, so it sits half a page into the small
     data the linker relaxes against. */
  __global_pointer$ = . + 0x800;
  .sdata     : { *(.sdata .sdata.*) }
  . = ALIGN(8);

  /* Not in the image -- objcopy drops it, and crt0 clears it instead. */
  .bss       : {
    _bss_start = .;
    *(.sbss .sbss.*) *(.bss .bss.*) *(COMMON)
    . = ALIGN(8);
    _bss_end = .;
  }
  _end = .;

  /* The stack grows down from the end of RAM.  The top word itself is left
     unused: 0x80000000 + RAM_SIZE truncates back to word zero, which is code. */
  _stack_top = 0x80000000 + RAM_SIZE - 16;
}
