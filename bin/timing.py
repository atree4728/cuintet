#!/usr/bin/env python3
"""Summarise a nextpnr run: the fmax of each clock, and where its critical path goes."""

import json
import re
import sys
from itertools import groupby


def unit(cell):
    return cell.rsplit(".", 1)[0] if "." in cell else "(top)"


def domain(name):
    return re.sub(r"posedge |\$glbnet\$|\$TRELLIS_IO_IN", "", name)


def walk(path):
    for seg in path["path"]:
        source = seg.get("from") or seg["to"]
        sink = seg.get("to") or seg["from"]
        yield seg["delay"], unit(source["cell"]), seg.get("net") or sink["cell"]


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: timing.py OUT_DIR")
    with open(f"{sys.argv[1]}/report.json") as f:
        report = json.load(f)

    for clock, fmax in report["fmax"].items():
        print(
            f"{domain(clock):6}{fmax['achieved']:7.2f} MHz   "
            f"period{1000 / fmax['achieved']:6.2f} ns   "
            f"(target {fmax['constraint']:.0f} MHz)"
        )

    paths = report.get("critical_paths")
    if not paths:
        return
    path = max(paths, key=lambda p: sum(seg["delay"] for seg in p["path"]))
    hops = list(walk(path))
    print(
        f"\ncritical path   {domain(path['from'])} -> {domain(path['to'])}   "
        f"{sum(delay for delay, _, _ in hops):.2f} ns over {len(hops)} hops"
    )

    runs = [(name, list(run)) for name, run in groupby(hops, key=lambda hop: hop[1])]
    width = max(len(name) for name, _ in runs) + 2
    elapsed = 0.0
    for name, run in runs:
        spent = sum(delay for delay, _, _ in run)
        elapsed += spent
        _, _, net = max(run, key=lambda hop: hop[0])
        print(
            f"  {elapsed:6.2f} ns  {name:{width}}+{spent:5.2f}  {len(run):2} hops   {net.rsplit('.', 1)[-1]}"
        )


if __name__ == "__main__":
    main()
