# To add an FPGA:
#   1. write fpga/<name>/mod.just, declaring name, top_module, top_fn and entity
#      alongside the recipes of its toolchain (fpga/common.just provides hdl)
#   2. add one mod line below

mod tangnano9k "fpga/tangnano9k"
mod timing "fpga/timing"

default:
    @just --list --list-submodules

# Everything under build/ but the konata logs: those are the record of past runs.
clean:
    -@find build -mindepth 1 -maxdepth 1 ! -name konata -exec rm -rf {} + 2>/dev/null
