const std = @import("std");

pub const Module = @import("src/root.zig");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("Utils", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const ansi_utils = b.dependency("Ansi", .{
        .target = target,
        .optimize = optimize,
    });

    const build_utils = b.dependency("Build", .{
        .target = target,
        .optimize = optimize,
    });

    const SnapshotWriter = build_utils.artifact("snapshot-writer");
    const Templater = build_utils.artifact("templater");
    const LibJoiner = build_utils.artifact("libjoiner");
    const HeaderGen = build_utils.namedLazyPath("HeaderGen.zig");

    b.default_step.dependOn(&b.addInstallArtifact(SnapshotWriter, .{}).step);
    b.default_step.dependOn(&b.addInstallArtifact(Templater, .{}).step);
    b.default_step.dependOn(&b.addInstallArtifact(LibJoiner, .{}).step);
    b.default_step.dependOn(&b.addInstallFile(HeaderGen, "HeaderGen.zig").step);


    const extern_utils = b.dependency("Extern", .{
        .target = target,
        .optimize = optimize,
    });

    const misc_utils = b.dependency("Misc", .{
        .target = target,
        .optimize = optimize,
    });

    const text_utils = b.dependency("Text", .{
        .target = target,
        .optimize = optimize,
    });

    const type_utils = b.dependency("Type", .{
        .target = target,
        .optimize = optimize,
    });

    mod.addImport("Ansi", ansi_utils.module("Ansi"));
    mod.addImport("Build", build_utils.module("Build"));
    mod.addImport("Extern", extern_utils.module("Extern"));
    mod.addImport("Misc", misc_utils.module("Misc"));
    mod.addImport("Text", text_utils.module("Text"));
    mod.addImport("Type", type_utils.module("Type"));
}
