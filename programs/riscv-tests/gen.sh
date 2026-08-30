#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/../.." && pwd)
isa=$root/vendor/riscv-tests/isa
env=$root/vendor/riscv-tests/env
prefix=${RISCV_PREFIX:-riscv64-unknown-elf-}
out=$root/build/programs/riscv-tests

. "$root/programs/common/hex.sh"

suites='rv64ui rv64um rv64mi'
if [ "$#" -gt 0 ]; then
  suites=$1
  shift
fi
tests=$*

for suite in $suites; do
  if [ ! -d "$isa/$suite" ]; then
    echo "$0: $isa/$suite is missing; run 'git submodule update --init'" >&2
    exit 1
  fi

  names=$tests
  if [ -z "$names" ]; then
    for src in "$isa/$suite"/*.S; do
      base=${src##*/}
      names="$names ${base%.S}"
    done
  fi

  mkdir -p "$here/hex/$suite" "$out/$suite"

  for name in $names; do
    elf=$out/$suite/$name.elf
    hex=$here/hex/$suite/$suite-p-$name.hex

    if ! "${prefix}gcc" \
      -march=rv64im_zicsr -mabi=lp64 \
      -nostdlib -nostartfiles -static -fno-pic \
      -Wl,--no-warn-rwx-segments \
      -T "$env/p/link.ld" \
      -I "$env/p" -I "$env" -I "$isa/macros/scalar" \
      -o "$elf" "$isa/$suite/$name.S"; then
      echo "$suite-p-$name: skipped, does not assemble"
      rm -f "$hex" "$elf"
      continue
    fi

    elf2hex "$elf" "$hex"

    echo "$suite-p-$name: $(wc -l <"$hex" | tr -d ' ') words"
  done
done
