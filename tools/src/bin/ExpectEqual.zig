const std = @import("std");

pub const std_options = std.Options{
    .log_level = .warn,
};

const log = std.log.scoped(.@"expect-equal");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    if (args.len == 1) return;

    const source = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        const file = try std.fs.cwd().readFileAlloc(allocator, args[i], std.math.maxInt(usize));

        if (!std.mem.eql(u8, source, file)) {
            log.err("Files [{s}] and [{s}] are not equal", .{ args[1], args[i] });
            return error.FileMismatch;
        }
    }
}
