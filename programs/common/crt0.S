        .section .text.init
        .globl _start
_start:
        .irp    reg, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
        li      x\reg, 0
        .endr

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

