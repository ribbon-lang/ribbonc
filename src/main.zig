const std = @import("std");
const log = std.log.scoped(.ribbon);

const Rli = @import("Rli");

const zig_builtin = @import("builtin");

pub const std_options = std.Options {
    .log_level = .warn,
};

const SCRIPT_PATHS = &[_][]const u8 {
    "src/scripts/root.bb",
};

const COMPTIME_DIR = "src/";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();
    const stdout = std.io.getStdOut().writer();

    const rli = try Rli.init(allocator, std.fs.cwd(), stdout.any(), Rli.Builtin.AllEnvs, &.{});
    rli.readFileCallback = readFile;
    defer rli.deinit();

    for (SCRIPT_PATHS) |scriptPath| {
        _ = try rli.readFile(scriptPath);
    }
}

fn readFile(rli: *Rli, fileName: []const u8) Rli.Error![]const u8 {
    const file = std.fs.cwd().openFile(fileName, .{ .mode = .read_only }) catch |err| {
        log.err("could not open file [{s}]: {s}", .{ fileName, @errorName(err) });
        return err;
    };
    defer file.close();

    const reader = file.reader();

    return reader.readAllAlloc(rli.allocator, std.math.maxInt(usize));
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
