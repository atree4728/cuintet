#!/bin/sh
# Assemble riscv-tests into the hex images the unit tests load.
#
# The images are checked in, so `cabal test` needs no RISC-V toolchain; run
# this only when the test list or the environment in env/ changes.
#
#   ./tests/riscv-tests/gen.sh            # the whole list below
#   ./tests/riscv-tests/gen.sh add addi   # just these
#
# Override the toolchain with RISCV_PREFIX=riscv32-unknown-elf- if needed.

set -eu

# fence_i needs Zifencei and ma_data needs misaligned accesses; neither is
# implemented, so they are left out of the default list rather than expected
# to fail.
DEFAULT_TESTS="
simple
add addi and andi auipc
beq bge bgeu blt bltu bne
jal jalr
lb lbu lh lhu lw ld_st lui
or ori
sb sh sw st_ld
sll slli slt slti sltiu sltu
sra srai srl srli
sub xor xori
"

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/../.." && pwd)
isa=$root/vendor/riscv-tests/isa
prefix=${RISCV_PREFIX:-riscv64-unknown-elf-}

if [ ! -d "$isa/rv32ui" ]; then
  echo "$0: $isa/rv32ui is missing; run 'git submodule update --init'" >&2
  exit 1
fi

if [ "$#" -gt 0 ]; then
  tests=$*
else
  tests=$DEFAULT_TESTS
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$here/hex"

for name in $tests; do
  elf=$work/$name.elf
  bin=$work/$name.bin

  # -I "$here/env" comes first so our riscv_test.h shadows the one in env/p.
  "${prefix}gcc" \
    -march=rv32i_zicsr -mabi=ilp32 \
    -nostdlib -nostartfiles -static -fno-pic \
    -Wl,--no-warn-rwx-segments \
    -T "$here/env/link.ld" \
    -I "$here/env" -I "$isa/macros/scalar" \
    -o "$elf" "$isa/rv32ui/$name.S"

  "${prefix}objcopy" -O binary "$elf" "$bin"

  # Pad to a whole word so the last line of hex is complete.
  size=$(wc -c < "$bin" | tr -d ' ')
  pad=$(( (4 - size % 4) % 4 ))
  if [ "$pad" -gt 0 ]; then
    dd if=/dev/zero bs=1 count="$pad" >> "$bin" 2>/dev/null
  fi

  # od reads each 4 bytes as a host-endian word, which matches RISC-V on any
  # little-endian host.  One word per line, low address first.
  od -An -tx4 -v "$bin" | tr -s ' ' '\n' | grep -v '^$' > "$here/hex/rv32ui-p-$name.hex"

  echo "rv32ui-p-$name: $(wc -l < "$here/hex/rv32ui-p-$name.hex" | tr -d ' ') words"
done
