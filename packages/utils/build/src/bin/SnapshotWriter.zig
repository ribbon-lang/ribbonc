const std = @import("std");
const Snapshot = @import("Snapshot").Snapshot;

const log = std.log.scoped(.@"snapshot-writer");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    const args = try std.process.argsAlloc(allocator);

    var map = try Snapshot.Map.init(allocator);

    const files = args[1..];

    var i: usize = 0;
    while (i < files.len) : (i += 4) {
        const kind = files[i];
        const name = files[i + 1];
        const pair = Snapshot.StrPair { .out = files[i + 2], .err = files[i + 3] };

        if (map.contains(name)) {
            log.err("duplicate snapshot name `{s}`", .{name});
            return error.DuplicateSnapshotName;
        }

        const snapshot: Snapshot =
            if (std.mem.eql(u8, kind, "-f") or std.mem.eql(u8, kind, "-file")) .{ .Files = pair } else if (std.mem.eql(u8, kind, "-t") or std.mem.eql(u8, kind, "-text")) .{ .Text = pair } else {
            log.err("expected `-f`/`-file` or `-t`/`-text`, got `{s}`", .{kind});
            return error.InvalidSnapshotKind;
        };

        try map.put(name, snapshot);
    }

    const out = std.io.getStdOut().writer();

    try map.writeMap(out);
}
