#!/bin/bash

zig build release -- x86_64-linux-gnu

valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes ./zig-out/x86_64-linux-gnu-release-fast/rvm

rm cachegrind.out.*
