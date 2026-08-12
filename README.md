# Cuintet

[![CI](https://github.com/atree4728/cuintet/actions/workflows/ci.yml/badge.svg)](https://github.com/atree4728/cuintet/actions/workflows/ci.yml)

*Cuintet* is a *5*-stage pipelined RISC-V CPU written in *Clash*, which implements `RV64I_Zicsr`.

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

To open the REPL, use:

```
cabal run clashi
```

To see the document, use:

```
cabal haddock --open
```

To log the core as Kanata format, use:

```
cabal run konata
```

### riscv-tests

`unittests` also runs the `rv64ui-p-*` and `rv64um-p-*` suites from
[riscv-tests](https://github.com/riscv-software-src/riscv-tests). The images are
assembled ahead of time and checked in under `tests/riscv-tests/hex/`, so no
RISC-V toolchain is needed to run them.

Regenerating them is only necessary after changing the test list or the
environment in `tests/riscv-tests/env/`:

```sh
git submodule update --init
./tests/riscv-tests/gen.sh            # every suite
./tests/riscv-tests/gen.sh rv64um     # one suite
./tests/riscv-tests/gen.sh rv64ui add # one test
```

### CoreMark

[CoreMark](https://github.com/eembc/coremark) runs bare-metal on the core, one
iteration of the default 2000-byte workload:

```sh
cabal run coremark
```

The core has no I/O, so the port in `bench/coremark/` stubs out `ee_printf` and
CoreMark reports nothing: `crt0.S` executes `ecall` once `main` has returned, and
the driver halts there. Reaching that `ecall` is what says the run completed.

The image is checked in as `bench/coremark/coremark.hex`, so running it needs no
RISC-V toolchain. Rebuild it after changing the port:

```sh
git submodule update --init
./bench/coremark/gen.sh
```

Two things the port has to do for this core in particular:

- `crt0.S` clears the integer registers, which start out undefined. A register
  the program never writes -- the vararg `a1`-`a7` that the `ee_printf` stub
  spills, say -- would otherwise put undefined bytes in memory.
- `.text` ends in a run of NOPs. IF runs ahead of the instruction that redirects
  it, so the `ret` ending the last function is followed into whatever comes
  next, and the decoder rejects an opcode it does not name.


## Synthesis

To compile the project to SystemVerilog, run:

```bash
cabal run clash -- Cuintet --systemverilog
```

You can find the SystemVerilog files in `systemverilog/`.

Synthesising for the Tang Nano 9K needs [just](https://github.com/casey/just) and
[oss-cad-suite](https://github.com/YosysHQ/oss-cad-suite-build) on `PATH`:

```bash
just prog   # build the bitstream and load it into SRAM
just flash  # build the bitstream and write it to the on-board flash
```
