# Turn a linked ELF into the hex listing `Cuintet.Debug.Image.hexImage` parses.
#
# Sourced by the gen.sh scripts beside it, which have already resolved $prefix:
#
#   . "$root/programs/common/hex.sh"
#   elf2hex prog.elf prog.hex

elf2hex() {
  _elf=$1
  _hex=$2
  _bin=$(mktemp)

  "${prefix}objcopy" -O binary "$_elf" "$_bin"

  # Pad to a whole word so the last line of hex is complete.
  _size=$(wc -c <"$_bin" | tr -d ' ')
  _pad=$(((4 - _size % 4) % 4))
  if [ "$_pad" -gt 0 ]; then
    dd if=/dev/zero bs=1 count="$_pad" >>"$_bin" 2>/dev/null
  fi

  # od reads each 4 bytes as a host-endian word, which matches RISC-V on any
  # little-endian host.  One word per line, low address first.
  od -An -tx4 -v "$_bin" | tr -s ' ' '\n' | grep -v '^$' >"$_hex"

  rm -f "$_bin"
}
