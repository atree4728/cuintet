#!/bin/sh
# Build CoreMark into the hex image the bench suite loads.
#
# The image is checked in, so `cabal run bench` needs no RISC-V toolchain; run
# this only after changing the port in this directory or the submodule.
#
#   ./programs/coremark/gen.sh
#
# Override the toolchain with RISCV_PREFIX=riscv64-unknown-elf- if needed.

set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/../.." && pwd)
common=$root/programs/common
src=$root/vendor/coremark
prefix=${RISCV_PREFIX:-riscv64-unknown-elf-}
hex=$here/hex/coremark.hex

. "$common/hex.sh"

if [ ! -f "$src/core_main.c" ]; then
  echo "$0: $src is missing; run 'git submodule update --init'" >&2
  exit 1
fi

# One iteration is all a simulator can afford; the run is still the same work
# CoreMark always does, so the count is comparable per iteration.  The three
# CRCs it validates are captured on the first iteration only, so the count does
# not weaken the check.
iterations=${ITERATIONS:-1}

# -mcmodel=medany because the image is linked at 0x80000000, out of reach of
# medlow's lui/addi pair.  -I "$here" comes first so the port in this directory
# shadows the barebones one.
flags="-march=rv64im_zicsr -mabi=lp64 -mcmodel=medany -O3"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mkdir -p "$here/hex"

# CoreMark validates its run by comparing three CRCs against known values and
# counting the mismatches into `total_errors`, but reports that only through
# ee_printf -- which this port drops -- and returns the constant MAIN_RETURN_VAL
# from main().  The testbench sees nothing but a0, so a broken core would look
# like a pass.  Rewrite main's two returns to hand back the count instead;
# `total_errors` is in scope at both.  The early one bails out before anything
# runs, so it gets a failure code of its own.
#
# The copy also matters for the include path: from $work, `#include "coremark.h"`
# no longer finds the submodule's own copy sitting beside core_main.c.
#
# The second rewrite waives CoreMark's reporting rule that a run shorter than
# ten seconds does not count, which it enforces by adding to the same
# total_errors.  Ten seconds of simulated time is 10^9 cycles, which no
# simulator here is going to reach, and the rule says nothing about whether the
# computation came out right.  Waiving it keeps a0 to the CRC and data-type
# checks -- and means the number this prints is not a publishable CoreMark
# score, which it was never going to be.
sed \
  -e '/list_head structure too big/{n;s/return MAIN_RETURN_VAL;/return -2;/;}' \
  -e 's/return MAIN_RETURN_VAL;/return total_errors;/' \
  -e 's/time_in_secs(total_time) < 10/0 \/* cuintet: rule waived, see gen.sh *\//' \
  "$src/core_main.c" >"$work/core_main.c"

if grep -q MAIN_RETURN_VAL "$work/core_main.c" ||
  ! grep -q 'return total_errors;' "$work/core_main.c" ||
  ! grep -q 'rule waived' "$work/core_main.c"; then
  echo "$0: could not rewrite core_main.c; has the submodule moved?" >&2
  exit 1
fi

"${prefix}gcc" \
  $flags \
  -nostdlib -nostartfiles -ffreestanding \
  -DITERATIONS="$iterations" \
  -DFLAGS_STR="\"$flags\"" \
  -I "$here" -I "$src" \
  -T "$common/link.ld" -Wl,--no-warn-rwx-segments \
  -o "$work/coremark.elf" \
  "$common/crt0.S" \
  "$src/core_list_join.c" \
  "$work/core_main.c" \
  "$src/core_matrix.c" \
  "$src/core_state.c" \
  "$src/core_util.c" \
  "$here/core_portme.c" \
  "$here/ee_printf.c"

elf2hex "$work/coremark.elf" "$hex"

"${prefix}size" "$work/coremark.elf"
echo "coremark.hex: $(wc -l <"$hex" | tr -d ' ') words"
