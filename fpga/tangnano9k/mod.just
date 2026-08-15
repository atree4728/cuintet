set working-directory := "../.."

import "../common.just"

name := "tangnano9k"
top_module := "Cuintet.Top.TangNano9k"
top_fn := "tangnano9k"
entity := "cuintet"

cst := "fpga/tangnano9k/cuintet.cst"
device := "GW1NR-LV9QN88PC6/I5"
family := "GW1N-9C"
board := "tangnano9k"
freq := "27"

# SystemVerilog -> JSON netlist
synth: hdl
    mkdir -p {{ out }}
    yosys -m slang -p 'read_slang --top {{ entity }} {{ sv }}; synth_gowin -json {{ out }}/{{ entity }}.json'

# place & route
pnr: synth
    nextpnr-himbaechel --json {{ out }}/{{ entity }}.json --write {{ out }}/{{ entity }}.pnr.json --device {{ device }} --vopt family={{ family }} --vopt cst={{ cst }} --freq {{ freq }}

# JSON -> bitstream
pack: pnr
    gowin_pack -d {{ family }} -o {{ out }}/{{ entity }}.fs {{ out }}/{{ entity }}.pnr.json

# write to SRAM (lost on power cycle)
prog: pack
    openFPGALoader -b {{ board }} {{ out }}/{{ entity }}.fs

# write to the on-board flash
flash: pack
    openFPGALoader -b {{ board }} -f {{ out }}/{{ entity }}.fs
