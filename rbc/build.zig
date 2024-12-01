const std = @import("std");

pub fn build(b: *std.Build) !void {
    const defaultTarget = b.standardTargetOptions(.{});
    const defaultOptimize = b.standardOptimizeOption(.{});

    const ISA = b.dependency("ISA", .{
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    const Utils = b.dependency("Utils", .{
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    const Core = b.addModule("Core", .{
        .root_source_file = b.path("src/Core/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    Core.addImport("ISA", ISA.module("ISA"));
    Core.addImport("Utils", Utils.module("Utils"));

    const Builder = b.addModule("Builder", .{
        .root_source_file = b.path("src/Builder/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    Builder.addImport("Utils", Utils.module("Utils"));
    Builder.addImport("Core", Core);
}
