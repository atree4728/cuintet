# Startup for a bare cuintet: no bootloader, no traps and no I/O.
#
# The testbench halts at the ECALL below, so reaching it is what says the
# benchmark ran to completion.

        .section .text.init
        .globl _start
_start:
        # The register file starts out undefined, so a register the program
        # never writes -- the vararg a1-a7 that ee_printf spills, say -- would
        # put undefined bytes in memory, and a mis-speculated fetch of those
        # bytes is not a defined instruction.  riscv-tests clears the registers
        # for the same reason.
        li x1, 0;  li x2, 0;  li x3, 0;  li x4, 0
        li x5, 0;  li x6, 0;  li x7, 0;  li x8, 0
        li x9, 0;  li x10, 0; li x11, 0; li x12, 0
        li x13, 0; li x14, 0; li x15, 0; li x16, 0
        li x17, 0; li x18, 0; li x19, 0; li x20, 0
        li x21, 0; li x22, 0; li x23, 0; li x24, 0
        li x25, 0; li x26, 0; li x27, 0; li x28, 0
        li x29, 0; li x30, 0; li x31, 0

        .option push
        .option norelax
        la      gp, __global_pointer$
        .option pop
        la      sp, _stack_top

        # Nothing is expected to trap.  Spin if one does, so the driver reports
        # it as a timeout rather than restarting at _start via a zero mtvec.
        la      t0, trap_spin
        csrw    mtvec, t0

        # .bss is not in the image; both bounds are 8-byte aligned.
        la      t0, _bss_start
        la      t1, _bss_end
1:      bgeu    t0, t1, 2f
        sd      zero, 0(t0)
        addi    t0, t0, 8
        j       1b

2:      call    main
        ecall                   # main's return value is left in a0

        .align 2
trap_spin:
        j       trap_spin

# IF runs ahead of the instruction that redirects it, so the `ret` ending the
# last function in .text is followed into whatever the linker put next.  The
# decoder rejects an opcode it does not name, and .rodata is not instructions,
# so .text ends in more NOPs than the pipeline can be ahead by.  Nothing
# branches here; it is only ever fetched down the wrong path.
        .section .guard, "ax", @progbits
        .rept 64
        nop
        .endr
