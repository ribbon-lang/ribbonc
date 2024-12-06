#!/bin/bash

 zig build release -DmaximumInlining=false -- x86_64-linux-gnu

sudo perf record -F 9000 -a -g -- ./zig-out/x86_64-linux-gnu-release-fast/rvm
sudo perf script > out.perf
stackcollapse-perf out.perf > out.folded
flamegraph out.folded > rvm.svg

rm -f perf.data
rm out.perf
rm out.folded
