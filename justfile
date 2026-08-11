hdl_dir := "systemverilog/Cuintet.tangnano9k"
out_dir := "build/tangnano9k"
cst := "fpga/tangnano9k/cuintet.cst"

device := "GW1NR-LV9QN88PC6/I5"
family := "GW1N-9C"
board := "tangnano9k"
freq := "27"

default:
    @just --list

# Clash -> SystemVerilog
hdl:
    cabal run clash -fclash-debug-transformations -- Cuintet --systemverilog

# SystemVerilog -> JSON netlist
synth: hdl
    mkdir -p {{ out_dir }}
    yosys -m slang -p 'read_slang --top cuintet {{ hdl_dir }}/cuintet_types.sv {{ hdl_dir }}/cuintet.sv; synth_gowin -json {{ out_dir }}/cuintet.json'

# place & route
pnr: synth
    nextpnr-himbaechel --json {{ out_dir }}/cuintet.json --write {{ out_dir }}/cuintet.pnr.json --device {{ device }} --vopt family={{ family }} --vopt cst={{ cst }} --freq {{ freq }}

# JSON -> bitstream
pack: pnr
    gowin_pack -d {{ family }} -o {{ out_dir }}/cuintet.fs {{ out_dir }}/cuintet.pnr.json

# write to SRAM (lost on power cycle)
prog: pack
    openFPGALoader -b {{ board }} {{ out_dir }}/cuintet.fs

# write to the on-board flash
flash: pack
    openFPGALoader -b {{ board }} -f {{ out_dir }}/cuintet.fs

clean:
    rm -rf {{ out_dir }}
