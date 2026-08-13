# Startup for a bare cuintet: no bootloader, no traps and no I/O.
#
# The testbench halts at the ECALL below, so reaching it is what says the
# benchmark ran to completion.

        .section .text.init
        .globl _start
_start:
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
