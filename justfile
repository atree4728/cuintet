# To add an FPGA:
#   1. write fpga/<name>/mod.just, declaring name, top_module, top_fn and entity
#      alongside the recipes of its toolchain (fpga/common.just provides hdl)
#   2. add one mod line below

mod tangnano9k "fpga/tangnano9k"
mod timing "fpga/timing"

default:
    @just --list --list-submodules

# Log a run as a Konata trace, named for the image and the revision it came from.
konata image:
    #!/bin/sh
    set -eu
    rev=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
    [ -z "$(git status --porcelain 2>/dev/null)" ] || rev="$rev-dirty"
    log="build/konata/{{ file_stem(image) }}-$rev.kanata.log"
    mkdir -p build/konata
    cabal run -v0 konata -- {{ image }} >"$log"
    echo "$log: written" >&2

# Diff a linked ELF's retire trace against spike's commit log.
tracediff elf:
    cabal run -v0 tracediff -- {{ elf }}

# Everything under build/ but the konata logs: those are the record of past runs.
clean:
    -@find build -mindepth 1 -maxdepth 1 ! -name konata -exec rm -rf {} + 2>/dev/null
