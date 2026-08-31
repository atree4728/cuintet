#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/../.." && pwd)
common=$root/programs/common
src=$root/vendor/coremark
prefix=${RISCV_PREFIX:-riscv64-unknown-elf-}
bin=$here/bin/coremark.bin
elf=$root/build/programs/coremark/coremark.elf

if [ ! -f "$src/core_main.c" ]; then
  echo "$0: $src is missing; run 'git submodule update --init'" >&2
  exit 1
fi

iterations=${ITERATIONS:-1}

flags="-march=rv64im_zicsr -mabi=lp64 -mcmodel=medany -O3"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mkdir -p "$here/bin" "${elf%/*}"

sed \
  -e '/list_head structure too big/{n;s/return MAIN_RETURN_VAL;/return -2;/;}' \
  -e 's/return MAIN_RETURN_VAL;/return total_errors;/' \
  "$src/core_main.c" >"$work/core_main.c"

if grep -q MAIN_RETURN_VAL "$work/core_main.c" ||
  ! grep -q 'return total_errors;' "$work/core_main.c"; then
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
  -o "$elf" \
  "$common/crt0.S" \
  "$src/core_list_join.c" \
  "$work/core_main.c" \
  "$src/core_matrix.c" \
  "$src/core_state.c" \
  "$src/core_util.c" \
  "$here/core_portme.c" \
  "$here/ee_printf.c"

"${prefix}objcopy" -O binary "$elf" "$bin"

"${prefix}size" "$elf"
echo "coremark.bin: $(wc -c <"$bin" | tr -d ' ') bytes"
