OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{
  . = 0x80000000;
  .text   : { *(.text.init) *(.text .text.*) }
  .rodata : { *(.rodata .rodata.*) }
  .data   : { *(.data .data.*) __global_pointer$ = . + 0x800; *(.sdata .sdata.*) }
  .bss (NOLOAD) : ALIGN(8) {
    _bss_start = .;
    *(.sbss .sbss.*) *(.bss .bss.*) *(COMMON)
    . = ALIGN(8);
    _bss_end = .;
  }
  _stack_top = 0x80000000 + 128K - 16;
}
