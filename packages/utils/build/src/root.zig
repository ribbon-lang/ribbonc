const std = @import("std");
const Build = std.Build;

pub const Templater = @import("./bin/Templater.zig");

pub const Compilation = @import("./Compilation.zig");
pub const SourceTree = @import("./SourceTree.zig");
pub const Manifest = @import("./Manifest.zig");
pub const HeaderGenUtils = @import("./HeaderGenUtils.zig");
pub const Snapshot = @import("./Snapshot.zig").Snapshot;

var headerGenSource: ?Build.LazyPath = null;

pub fn makeHeaderGenSource(b: *Build) Build.LazyPath {
    if (headerGenSource) |hgs| return hgs;

    const run = b.addSystemCommand(&[_][]const u8{ "echo", @embedFile("./bin/HeaderGen.zig") });

    const hgs = run.captureStdOut();

    headerGenSource = hgs;

    return hgs;
}

pub fn makeHeaderGen(b: *Build, module: *Build.Module) !*Build.Step.Compile {
    const nativeTarget = b.resolveTargetQuery(.{});
    const name = try std.fmt.allocPrint(b.allocator, "headergen:{s}", .{std.fs.path.stem(module.root_source_file.?.getDisplayName())});
    const exe = b.addExecutable(.{
        .name = name,
        .root_source_file = makeHeaderGenSource(b),
        .target = nativeTarget,
        .optimize = .Debug,
    });
    exe.root_module.addImport(HeaderGenUtils.SOURCE_MODULE_NAME, module);
    return exe;
}
