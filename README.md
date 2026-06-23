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

To open the REPL, use:

```
cabal run clashi
```

## Compiling to HDL

To compile the project to SystemVerilog, run:

```bash
cabal run clash -- Cuintet.Cpu --systemverilog
```

You can find the SystemVerilog files in `systemverilog/`.

Also, Clash is able to compile into either VHDL or Verilog HDL.
