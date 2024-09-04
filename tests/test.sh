#!/bin/sh

# ensure the dependencies are installed
zig build release -- x86_64-linux-gnu

gcc \
    ./tests/test.c \
    -o./zig-out/gcc-test \
    -I./zig-out/x86_64-linux-gnu-release-safe/include \
    -L./zig-out/x86_64-linux-gnu-release-safe \
    -lribbonc

./zig-out/gcc-test
