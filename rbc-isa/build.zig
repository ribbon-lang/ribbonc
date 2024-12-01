const std = @import("std");

pub fn build(b: *std.Build) !void {
    const defaultTarget = b.standardTargetOptions(.{});
    const defaultOptimize = b.standardOptimizeOption(.{});

    const zigBuildUtils = b.dependency("ZigBuildUtils", .{
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    const ISA = b.addModule("ISA", .{
        .root_source_file = b.path("src/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    const readmeStep = b.step("readme", "Generate README.md");

    const templater = b.addExecutable(.{
        .name = "templater",
        .root_source_file = b.path("src/Templater.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    templater.root_module.addImport("ZigBuildUtils", zigBuildUtils.module("ZigBuildUtils"));
    templater.root_module.addImport("ISA", ISA);

    const runTemplater = b.addRunArtifact(zigBuildUtils.artifact("templater"));

    const templatePath = b.path("README.template.md");

    runTemplater.has_side_effects = true;

    runTemplater.addFileInput(templatePath);
    runTemplater.addArg("README.template.md");

    runTemplater.addArg("-no-static");

    runTemplater.addArg("ISA");
    runTemplater.addArtifactArg(templater);

    const installReadme = b.addUpdateSourceFiles();
    installReadme.addCopyFileToSource(runTemplater.captureStdOut(), "README.md");

    readmeStep.dependOn(&installReadme.step);
}
