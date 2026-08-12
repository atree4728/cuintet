#!/bin/sh
# Build CoreMark into the hex image the driver loads.
#
# The image is checked in, so `cabal run coremark` needs no RISC-V toolchain;
# run this only after changing the port in this directory or the submodule.
#
#   ./bench/coremark/gen.sh
#
# Override the toolchain with RISCV_PREFIX=riscv64-unknown-elf- if needed.

set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/../.." && pwd)
src=$root/vendor/coremark
prefix=${RISCV_PREFIX:-riscv64-unknown-elf-}
hex=$here/coremark.hex

if [ ! -f "$src/core_main.c" ]; then
  echo "$0: $src is missing; run 'git submodule update --init'" >&2
  exit 1
fi

# One iteration is all a simulator can afford; the run is still the same work
# CoreMark always does, so the count is comparable per iteration.
iterations=${ITERATIONS:-1}

# -mcmodel=medany because the image is linked at 0x80000000, out of reach of
# medlow's lui/addi pair.  -I "$here" comes first so the port in this directory
# shadows the barebones one.
flags="-march=rv64im_zicsr -mabi=lp64 -mcmodel=medany -O2"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

"${prefix}gcc" \
  $flags \
  -nostdlib -nostartfiles -ffreestanding \
  -DITERATIONS="$iterations" \
  -DFLAGS_STR="\"$flags\"" \
  -I "$here" -I "$src" \
  -T "$here/link.ld" -Wl,--no-warn-rwx-segments \
  -o "$work/coremark.elf" \
  "$here/crt0.S" \
  "$src/core_list_join.c" \
  "$src/core_main.c" \
  "$src/core_matrix.c" \
  "$src/core_state.c" \
  "$src/core_util.c" \
  "$here/core_portme.c" \
  "$here/ee_printf.c"

"${prefix}objcopy" -O binary "$work/coremark.elf" "$work/coremark.bin"

# Pad to a whole word so the last line of hex is complete.
size=$(wc -c <"$work/coremark.bin" | tr -d ' ')
pad=$(((4 - size % 4) % 4))
if [ "$pad" -gt 0 ]; then
  dd if=/dev/zero bs=1 count="$pad" >>"$work/coremark.bin" 2>/dev/null
fi

# od reads each 4 bytes as a host-endian word, which matches RISC-V on any
# little-endian host.  One word per line, low address first.
od -An -tx4 -v "$work/coremark.bin" | tr -s ' ' '\n' | grep -v '^$' >"$hex"

"${prefix}size" "$work/coremark.elf"
echo "coremark.hex: $(wc -l <"$hex" | tr -d ' ') words"
