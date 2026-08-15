# Not a board: a synthesis target that exists only to measure area and fmax.
# The device is simply one large enough that the design always fits.

set working-directory := "../.."

import "../common.just"

name := "timing"
top_module := "Cuintet.Top.Timing"
top_fn := "timing"
entity := "cuintet_timing"

device := "--85k --package CABGA381"
freq := "200"
seed := "1"

# area, without place & route (seconds)
synth: hdl
    mkdir -p {{ out }}
    yosys -q -m slang -l {{ out }}/yosys.log -p 'read_slang --top {{ entity }} {{ sv }}; synth_ecp5 -json {{ out }}/{{ entity }}.json; tee -q -o {{ out }}/area.txt stat'
    @cat {{ out }}/area.txt

# max frequency and critical path (minutes)
fmax: synth
    nextpnr-ecp5 -q {{ device }} --json {{ out }}/{{ entity }}.json --freq {{ freq }} --seed {{ seed }} --timing-allow-fail --report {{ out }}/report.json -l {{ out }}/nextpnr.log
    @just timing::report

# the last fmax run, summarised; reads the artefacts, so it does not re-place
report:
    @cabal run timing -- {{ out }} {{ src }}
