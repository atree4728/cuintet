# Cuintet

[![CI](https://github.com/atree4728/cuintet/actions/workflows/ci.yml/badge.svg)](https://github.com/atree4728/cuintet/actions/workflows/ci.yml)

*Cuintet* is a *5*-stage pipelined RISC-V CPU written in *Clash*, which implements `RV64IM_Zicsr`.

## Building and testing

To build the project, use:

```sh
cabal build
```

To run the tests defined in `tests/` and `bench/`, use:

```bash
cabal run unittests
cabal run doctests
cabal run bench
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
