const std = @import("std");

const zig_builtin = @import("builtin");

const Config = @import("Config");
const MiscUtils = @import("Utils").Misc;
const CLIMetaData = @import("CLIMetaData");
const TextUtils = @import("Utils").Text;
const ANSI = @import("Utils").ANSI;
const Core = @import("Core");
const Builder = @import("Rbc:Builder");
const log = std.log.scoped(.rvm);

pub const std_options = std.Options {
    .log_level = Config.LOG_LEVEL,
    .logFn = MiscUtils.FilteredLogger(Config.LOG_SCOPES),
};

const Error = MiscUtils.IOError || std.mem.Allocator.Error || CLIMetaData.CLIError || Core.Fiber.Trap || Builder.Error || error {
    TestExpectedEqual,
};

// pub const main = main: {
//     if (zig_builtin.mode == .Debug) {
//         break :main entry;
//     } else {
//         break :main struct {
//             fn fun() u8 {
//                 entry() catch {
//                     return 1;
//                 };

//                 return 0;
//             }
//         }.fun;
//     }
// };

// fn entry() Error!void {
//     if (zig_builtin.os.tag == .windows) {
//         const succ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
//         if (succ == 0) {
//             const lastErr = std.os.windows.kernel32.GetLastError();
//             const safeToPrint = @intFromEnum(lastErr) >= @intFromEnum(std.os.windows.Win32Error.SUCCESS) and @intFromEnum(lastErr) <= @intFromEnum(std.os.windows.Win32Error.IO_REISSUE_AS_CACHED);

//             if (safeToPrint) {
//                 log.warn("failed to set console output code page to UTF-8, error was {s}", .{@tagName(lastErr)});
//             } else {
//                 log.warn("failed to set console output code page to UTF-8, error was {}", .{@intFromEnum(lastErr)});
//             }
//         }
//     }

//     const stderr = std.io.getStdErr().writer();

//     var GPA = std.heap.GeneralPurposeAllocator(.{}){};
//     defer _ = GPA.deinit();

//     const gpa = GPA.allocator();

//     const args = std.process.argsAlloc(gpa) catch |err| {
//         log.err("failed to get command line arguments: {}", .{err});
//         return error.Unexpected;
//     };
//     defer gpa.free(args);

//     const endOfOwnArgs =
//         for (args, 0..) |arg, i| {
//             if (std.mem.eql(u8, arg, "--")) {
//                 break i;
//             }
//         } else args.len;

//     const scriptArgs = args[@min(endOfOwnArgs + 1, args.len)..];

//     const argsResult = try CLIMetaData.processArgs(gpa, args[1..endOfOwnArgs]);
//     defer argsResult.deinit();

//     switch (argsResult) {
//         .exit => return,
//         .execute => |x| {
//             try earlyTesting(gpa, stderr, x.rootFiles, scriptArgs);
//         },
//     }
// }



pub fn main() Error!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const context = try Core.Context.init(arena.allocator());
    // defer context.deinit();

    var builder = try Builder.init(arena.allocator());

    // const out_global = try builder.globalNative(@as(i64, 0));

    const one = try builder.globalNative(@as(i64, 1));
    const two = try builder.globalNative(@as(i64, 2));

    const func = try builder.main();


    const arg = try func.arg();
    const cond = try func.local();
    const two_loaded = try func.local();
    const one_loaded = try func.local();
    try func.entry.read_global_64(two, two_loaded);
    try func.entry.read_global_64(one, one_loaded);
    try func.entry.s_lt_64(arg, two_loaded, cond);
    const thenBlock, const elseBlock = try func.entry.if_nz(cond);

    try func.entry.trap();

    try thenBlock.ret_v(arg);

    const a = try func.local();
    try elseBlock.i_sub_64(arg, one_loaded, a);
    try elseBlock.call_im_v(func, a, .{a});

    const b = try func.local();
    try elseBlock.i_sub_64(arg, two_loaded, b);
    try elseBlock.call_im_v(func, b, .{b});

    try elseBlock.i_add_64(a, b, a);
    try elseBlock.ret_v(a);

    const program = try builder.assemble(arena.allocator());
    // defer program.deinit(arena.allocator());


    const fiber = try Core.Fiber.init(context, &program, &[0] Core.Fiber.ForeignFunction {});
    defer fiber.deinit();

    const n: i64 = 32;

    const start = std.time.nanoTimestamp();

    const result = try fiber.invoke(i64, program.main, .{ n });

    const end = std.time.nanoTimestamp();

    const time = @as(f64, @floatFromInt(end - start)) / std.time.ns_per_s;


    try std.io.getStdOut().writer().print("result: {} (in {d:.3}s)\n", .{result, time});
    try std.testing.expectEqual(2178309, result);
}

// fn fib(n: i64) i64 {
//     return if (n < 2) n else fib(n - 1) + fib(n - 2);
// }


test {
    std.testing.refAllDeclsRecursive(@This());
}
