const std = @import("std");

const Extern = @import("Utils").Extern;
const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module provides functions for interacting with the file system.
    \\Many of these will likely be converted to prompts in the future.
    \\
    \\Also available here are the std-io constants `std-in`, `std-out`, and `std-err`,
    \\which refer to file descriptors usable by `read-file`, `write-file` and their related functions.
    \\
;

pub const Env = .{
    .{ "open-file", "open a file at the provided path; accepts mode symbol `'r`, `'w`, or `'rw`; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            comptime // since zig is still unstable this seems like a good sanity check...
            std.debug.assert(@intFromEnum(std.fs.File.OpenMode.read_only)  == 0
                         and @intFromEnum(std.fs.File.OpenMode.write_only) == 1
                         and @intFromEnum(std.fs.File.OpenMode.read_write) == 2);
            var rargs = [2]SExpr{ undefined, undefined };
            const rargsLen = try eval.resolveSmallList(args, 1, &rargs);
            const path = try eval.castStringSlice(at, rargs[0]);
            const mode: std.fs.File.OpenMode = mode: {
                if (rargsLen == 2) {
                    const m = try eval.castSymbolSlice(at, rargs[1]);
                    if (MiscUtils.find([]const u8, &[_][]const u8{ "r", "w", "rw" }, &m)) |i| {
                        break :mode @enumFromInt(i);
                    } else {
                        return eval.abort(Eval.Error.TypeError, at, "expected a file mode symbol of the set `r`, `w`, or `rw` for open-file, got {s}", .{m});
                    }
                } else {
                    break :mode .read_write;
                }
            };
            const file = openFile(path, mode) catch |err| {
                return eval.errorToException(at, err);
            };
            const filePtr = try eval.context.new(file);
            return try ExternFile(at, filePtr);
        }
    } },
    .{ "read-file", "read the content of a text file to a string; if a second parameter is given it is expected to be the length in bytes to read, otherwise reads the whole file; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = [2]SExpr{ undefined, undefined };
            const rargsLen = try eval.resolveSmallList(args, 1, &rargs);
            const file = try eval.castExternDataPtr(std.fs.File, at, rargs[0]);
            const reader = file.reader();
            const data = data: {
                if (rargsLen > 1) {
                    const len = rargs[1].castInt() orelse {
                        return eval.abort(Eval.Error.TypeError, rargs[1].getAttr(), "expected an integer (byte length) as the second argument to read-file, got {}", .{rargs[1].getTag()});
                    };
                    if (len < 0) {
                        return eval.abort(Eval.Error.TypeError, rargs[1].getAttr(), "expected a non-negative integer (byte length) as the second argument to read-file, got {}", .{len});
                    }
                    var buf = std.ArrayList(u8).init(eval.context.allocator);
                    const mem = try buf.addManyAt(0, @intCast(len));
                    const read_len = reader.readAll(mem) catch |err| {
                        return eval.errorToException(at, err);
                    };
                    buf.shrinkAndFree(read_len);
                    break :data buf.items;
                } else {
                    break :data reader.readAllAlloc(eval.context.allocator, std.math.maxInt(i64)) catch |err| {
                        return eval.errorToException(at, err);
                    };
                }
            };
            return try SExpr.StringPreallocated(at, data);
        }
    } },
    .{ "read-ln", "read a single line of a text file to a string; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const fileObj = try eval.resolve1(args);
            const file = try eval.castExternDataPtr(std.fs.File, at, fileObj);
            const reader = file.reader();
            const data = reader.readUntilDelimiterAlloc(eval.context.allocator, '\n', std.math.maxInt(i64)) catch |err| {
                return eval.errorToException(at, err);
            };
            return try SExpr.StringPreallocated(at, data);
        }
    } },
    .{ "write-file", "write a string to a file; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const file = try eval.castExternDataPtr(std.fs.File, at, rargs[0]);
            const data = try eval.castStringSlice(at, rargs[1]);
            const writer = file.writer();
            writer.writeAll(data) catch |err| {
                return eval.errorToException(at, err);
            };
            return try SExpr.Nil(at);
        }
    } },
    .{ "write-ln", "write an optional string to a file, then write a new line; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = [2]SExpr{ undefined, undefined };
            const rargsLen = try eval.resolveSmallList(args, 1, &rargs);
            const file = try eval.castExternDataPtr(std.fs.File, at, rargs[0]);
            const data = data: {
                if (rargsLen > 1) {
                    break :data try eval.castStringSlice(at, rargs[1]);
                } else {
                    break :data "";
                }
            };
            const writer = file.writer();
            writer.writeAll(data) catch |err| {
                return eval.errorToException(at, err);
            };
            writer.writeByte('\n') catch |err| {
                return eval.errorToException(at, err);
            };
            return try SExpr.Nil(at);
        }
    } },
    .{ "print", "stringify all* arguments with `'Display`, concatenate, then `write-file` with the resulting string; if the first parameter is a file, prints to that file instead of std-out; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            const fst = try rargs.next();
            const writer =
                if (if (fst) |fs| fs.castExternDataExactPtr(std.fs.File) else null) |f| f.writer() else f: {
                const writer = std.io.getStdOut().writer();
                if (fst) |fs| {
                    writer.print("{display}", .{fs}) catch |err| {
                        return eval.errorToException(at, err);
                    };
                }
                break :f writer;
            };
            while (try rargs.next()) |next| {
                writer.print("{display}", .{next}) catch |err| {
                    return eval.errorToException(at, err);
                };
            }
            return SExpr.Nil(at);
        }
    } },
    .{ "print-ln", "stringify all* arguments with `'Display`, concatenate, then `write-ln` with the resulting string; if the first parameter is a file, prints to that file instead of std-out; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            const fst = try rargs.next();
            const writer =
                if (if (fst) |fs| fs.castExternDataExactPtr(std.fs.File) else null) |f| f.writer() else f: {
                const writer = std.io.getStdOut().writer();
                if (fst) |fs| {
                    writer.print("{display}", .{fs}) catch |err| {
                        return eval.errorToException(at, err);
                    };
                }
                break :f writer;
            };
            if (fst != null) {
                while (try rargs.next()) |next| {
                    writer.print("{display}", .{next}) catch |err| {
                        return eval.errorToException(at, err);
                    };
                }
            }
            writer.writeByte('\n') catch |err| {
                return eval.errorToException(at, err);
            };
            return SExpr.Nil(at);
        }
    } },
    .{ "file-end", "get the cursor position that marks the end of a file; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const fileObj = try eval.resolve1(args);
            const file = try eval.castExternDataPtr(std.fs.File, at, fileObj);
            const end = file.getEndPos() catch |err| {
                return eval.errorToException(at, err);
            };
            if (end > @as(u64, @intCast(std.math.maxInt(i64)))) {
                return eval.abort(Eval.Error.RangeError, at, "file is too long to get its end cursor: {}", .{end});
            }
            return try SExpr.Int(at, @intCast(end));
        }
    } },
    .{ "file-cursor", "get the current cursor position in a file; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const fileObj = try eval.resolve1(args);
            const file = try eval.castExternDataPtr(std.fs.File, at, fileObj);
            const cursor = file.getPos() catch |err| {
                return eval.errorToException(at, err);
            };
            if (cursor > @as(u64, @intCast(std.math.maxInt(i64)))) {
                return eval.abort(Eval.Error.RangeError, at, "file is too long to get its end cursor: {}", .{cursor});
            }
            return try SExpr.Int(at, @intCast(cursor));
        }
    } },
    .{ "file-cursor!", "set the current cursor position in a file; prompts an exception if it fails", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const file = try eval.castExternDataPtr(std.fs.File, at, rargs[0]);
            const cursor = try eval.castInt(at, rargs[1]);
            if (cursor < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer for file-cursor!, got {}", .{cursor});
            }
            file.seekTo(@intCast(cursor)) catch |err| {
                return eval.errorToException(at, err);
            };
            return try SExpr.Nil(at);
        }
    } },
    .{ "std-in", "the input file constant", struct {
        pub fn init(at: *const Source.Attr) Eval.Result!SExpr {
            const file = std.io.getStdIn();
            const filePtr = try at.context.new(file);
            return try ExternFile(at, filePtr);
        }
    } },
    .{ "std-out", "the output file constant", struct {
        pub fn init(at: *const Source.Attr) Eval.Result!SExpr {
            const file = std.io.getStdOut();
            const filePtr = try at.context.new(file);
            return try ExternFile(at, filePtr);
        }
    } },
    .{ "std-err", "the error file constant", struct {
        pub fn init(at: *const Source.Attr) Eval.Result!SExpr {
            const file = std.io.getStdErr();
            const filePtr = try at.context.new(file);
            return try ExternFile(at, filePtr);
        }
    } },
};

fn openFile(path: []const u8, mode: std.fs.File.OpenMode) !std.fs.File {
    const cwd = std.fs.cwd();
    switch (mode) {
        .read_only => return cwd.openFile(path, .{ .mode = mode }),
        else => return cwd.createFile(path, .{ .read = mode == .read_write, .exclusive = true }) catch |err| {
            switch (err) {
                error.PathAlreadyExists => {
                    return cwd.openFile(path, .{ .mode = mode });
                },
                else => return err,
            }
        },
    }
}

pub fn ExternFile(attr: *const Source.Attr, file: *std.fs.File) !SExpr {
    const ZigBuiltin = @import("builtin");

    const FileVTable = SExpr.Types.ExternData.VTable(std.fs.File){
        .compare = struct {
            fn fun(self: *const std.fs.File, other: *const std.fs.File) callconv(.C) MiscUtils.Ordering {
                if (comptime ZigBuiltin.os.tag == .windows) {
                    return MiscUtils.compare(@intFromPtr(self.handle), @intFromPtr(other.handle));
                } else {
                    return MiscUtils.compare(self.handle, other.handle);
                }
            }
        }.fun,
        .hashWith = struct {
            fn fun(self: *const std.fs.File, hasher: *Extern.Hasher) callconv(.C) void {
                if (comptime ZigBuiltin.os.tag == .windows) {
                    MiscUtils.hashWith(hasher, @intFromPtr(self.handle));
                } else {
                    MiscUtils.hashWith(hasher, self.handle);
                }
            }
        }.fun,
        .format = struct {
            fn fun(self: *const std.fs.File, writer: Extern.Writer) callconv(.C) bool {
                return writer.print("[File {}]", .{self.handle});
            }
        }.fun,
        .finalizer = struct {
            fn fun(self: *std.fs.File) callconv(.C) void {
                if (self.handle == std.io.getStdIn().handle or self.handle == std.io.getStdOut().handle or self.handle == std.io.getStdErr().handle) {
                    return;
                }

                self.close();
            }
        }.fun,
    };

    return SExpr.ExternData(std.fs.File, attr, file, &FileVTable);
}
