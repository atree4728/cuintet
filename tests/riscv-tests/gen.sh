#!/bin/sh
# Assemble riscv-tests into the hex images the unit tests load.
#
# The images are checked in, so `cabal test` needs no RISC-V toolchain; run
# this only when the suite or the environment in env/ changes.
#
#   ./tests/riscv-tests/gen.sh            # every test in the suite
#   ./tests/riscv-tests/gen.sh add addi   # just these
#
# Tests the core cannot run yet are not filtered here: a test needing an
# extension outside -march below is reported as skipped, and one that
# assembles but fails is left for the testbench to reject, hex and all.
#
# Override the toolchain with RISCV_PREFIX=riscv64-unknown-elf- if needed.

set -eu

# The suite to assemble, naming both the source directory under isa/ and the
# hex/ subdirectory the images land in.
suite=rv64ui

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/../.." && pwd)
isa=$root/vendor/riscv-tests/isa
prefix=${RISCV_PREFIX:-riscv64-unknown-elf-}

if [ ! -d "$isa/$suite" ]; then
  echo "$0: $isa/$suite is missing; run 'git submodule update --init'" >&2
  exit 1
fi

if [ "$#" -gt 0 ]; then
  tests=$*
else
  tests=
  for src in "$isa/$suite"/*.S; do
    name=${src##*/}
    tests="$tests ${name%.S}"
  done
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$here/hex/$suite"

for name in $tests; do
  elf=$work/$name.elf
  bin=$work/$name.bin
  hex=$here/hex/$suite/$suite-p-$name.hex

  # -I "$here/env" comes first so our riscv_test.h shadows the one in env/p.
  if ! "${prefix}gcc" \
    -march=rv64i_zicsr -mabi=lp64 \
    -nostdlib -nostartfiles -static -fno-pic \
    -Wl,--no-warn-rwx-segments \
    -T "$here/env/link.ld" \
    -I "$here/env" -I "$isa/macros/scalar" \
    -o "$elf" "$isa/$suite/$name.S" 2>"$work/$name.log"; then
    sed "s/^/  /" "$work/$name.log" >&2
    echo "$suite-p-$name: skipped, does not assemble as rv64i_zicsr"
    rm -f "$hex"
    continue
  fi

  "${prefix}objcopy" -O binary "$elf" "$bin"

  # Pad to a whole word so the last line of hex is complete.
  size=$(wc -c <"$bin" | tr -d ' ')
  pad=$(((4 - size % 4) % 4))
  if [ "$pad" -gt 0 ]; then
    dd if=/dev/zero bs=1 count="$pad" >>"$bin" 2>/dev/null
  fi

  # od reads each 4 bytes as a host-endian word, which matches RISC-V on any
  # little-endian host.  One word per line, low address first.
  od -An -tx4 -v "$bin" | tr -s ' ' '\n' | grep -v '^$' >"$hex"

  echo "$suite-p-$name: $(wc -l <"$hex" | tr -d ' ') words"
done
