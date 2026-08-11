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
