#!/bin/bash

time zig build release -- x86_64-linux-gnu

sudo perf stat -d -r 100 ./zig-out/x86_64-linux-gnu-release-fast/rvm
