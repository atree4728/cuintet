hdl_dir := "build/systemverilog"
out_dir := "build/tangnano9k"
cst := "fpga/tangnano9k/cuintet.cst"

device := "GW1NR-LV9QN88PC6/I5"
family := "GW1N-9C"
board := "tangnano9k"
freq := "27"

# timing-only target: a device large enough that the design always fits
timing_dir := "build/timing"
timing_hdl := hdl_dir / "Cuintet.Top.Timing.timing"
timing_top := "cuintet_timing"
timing_device := "--85k --package CABGA381"
timing_freq := "200"
timing_seed := "1"

default:
    @just --list

# Clash -> SystemVerilog
hdl:
    cabal run clash -- Cuintet.Top.TangNano9k --systemverilog -fclash-hdldir {{ hdl_dir }}

# SystemVerilog -> JSON netlist
synth: hdl
    mkdir -p {{ out_dir }}
    yosys -m slang -p 'read_slang --top cuintet {{ hdl_dir }}/Cuintet.Top.TangNano9k.tangnano9k/cuintet_types.sv {{ hdl_dir }}/Cuintet.Top.TangNano9k.tangnano9k/cuintet.sv; synth_gowin -json {{ out_dir }}/cuintet.json'

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

# Clash -> SystemVerilog, for the timing-only top
timing-hdl:
    cabal run clash -- Cuintet.Top.Timing --systemverilog -fclash-hdldir {{ hdl_dir }}

# area, without place & route (seconds)
timing-synth: timing-hdl
    mkdir -p {{ timing_dir }}
    yosys -q -m slang -l {{ timing_dir }}/yosys.log -p 'read_slang --top {{ timing_top }} {{ timing_hdl }}/{{ timing_top }}_types.sv {{ timing_hdl }}/{{ timing_top }}.sv; synth_ecp5 -json {{ timing_dir }}/{{ timing_top }}.json; tee -q -o {{ timing_dir }}/area.txt stat'
    @cat {{ timing_dir }}/area.txt

# max frequency and critical path (minutes)
timing-fmax: timing-synth
    nextpnr-ecp5 -q {{ timing_device }} --json {{ timing_dir }}/{{ timing_top }}.json --freq {{ timing_freq }} --seed {{ timing_seed }} --timing-allow-fail --report {{ timing_dir }}/report.json -l {{ timing_dir }}/nextpnr.log
    @grep "Max frequency for clock" {{ timing_dir }}/nextpnr.log | tail -1
    @echo "critical path: {{ timing_dir }}/nextpnr.log"

clean:
    rm -rf {{ out_dir }} {{ timing_dir }}
