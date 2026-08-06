# Cuintet

A RISC-V CPU written in Clash.

## Building and testing

To build the project, use:

```sh
cabal build
```

To run the tests defined in `tests/`, use:

```bash
cabal run unittests
cabal run doctests
```

### riscv-tests

`unittests` also runs the `rv64ui-p-*` suite from
[riscv-tests](https://github.com/riscv-software-src/riscv-tests). The images are
assembled ahead of time and checked in under `tests/riscv-tests/hex/`, so no
RISC-V toolchain is needed to run them.

Regenerating them is only necessary after changing the test list or the
environment in `tests/riscv-tests/env/`:

```sh
git submodule update --init
./tests/riscv-tests/gen.sh
```

This needs `riscv64-unknown-elf-gcc` with `rv64i` multilib; override the
toolchain with `RISCV_PREFIX`.

`fence_i` and `ma_data` are left out, since Zifencei and misaligned accesses
are not implemented.

The tests are linked against a cut-down environment in `tests/riscv-tests/env/`,
because the upstream `env/p` initialises PMP, address translation and trap
delegation, and reports through the `tohost` MMIO word. Instead a test leaves
its verdict in `gp` and executes `ecall`, and the testbench halts there.

riscv-tests and the environment it is derived from are Copyright (c) 2012-2015,
The Regents of the University of California; see `tests/riscv-tests/LICENSE`.

To open the REPL, use:

```
cabal run clashi
```

To see the document, use:

```
cabal haddock --open
```

## Compiling to HDL

To compile the project to SystemVerilog, run:

```bash
cabal run clash -- Cuintet --systemverilog
```

You can find the SystemVerilog files in `systemverilog/`.

Also, Clash is able to compile into either VHDL or Verilog HDL.
