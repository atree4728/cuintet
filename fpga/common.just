# Shared by every target under fpga/. A target declares name, top_module,
# top_fn and entity; everything below is derived from those.

hdl_dir := "build/systemverilog"

# where Clash writes the HDL, and where the target's artefacts go
src := hdl_dir / top_module + "." + top_fn
out := "build" / name

# every module Clash emitted: the top entity, its types, and one per OPAQUE
# binding, so it is not a fixed list
sv := src / "*.sv"

# Clash -> SystemVerilog
hdl:
    cabal run clash -- {{ top_module }} --systemverilog -fclash-hdldir {{ hdl_dir }}
