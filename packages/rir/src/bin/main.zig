const std = @import("std");

const RirCore = @import("Rir:Core");


pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    const allocator = gpa.allocator();

    var ir = try RirCore.IR.init(allocator);
    defer ir.deinit();

    const module = try ir.module();

    const one = try module.globalFromNative(@as(i32, 1));

    const Incr = try module.typeIdFromNative(fn (i32) i32);
    const incr = try module.function(Incr);

    const entry = incr.entry();
    try entry.ref_local(0);
    try entry.ref_global(one.id);
    try entry.add();
    try entry.ret();

    std.debug.print("{}\n", .{entry});
}
