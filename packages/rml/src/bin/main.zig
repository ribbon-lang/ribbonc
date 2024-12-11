const std = @import("std");

const Rml = @import("Rml");
const log = std.log.scoped(.main);

pub const std_options = std.Options {
    .log_level = .info,
    // .log_scope_levels = &.{
    //     std.log.ScopeLevel {
    //         .level = .debug,
    //         .scope = .refcount,
    //     }
    // },
    // .log_scope_levels = &.{
    //     std.log.ScopeLevel {
    //         .level = .debug,
    //         .scope = .parsing,
    //     }
    // },
    // .log_scope_levels = &.{
    //     std.log.ScopeLevel {
    //         .level = .debug,
    //         .scope = .evaluation,
    //     }
    // },
};

pub fn main () !void {
    log.debug("init", .{});

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer {
        log.debug("Deinitializing gpa", .{});
        _ = gpa.deinit();
    }

    const rml: *Rml = try .init(gpa.allocator(), null, null, &.{});
    log.debug("rml initialized", .{});
    defer {
        log.debug("Deinitializing Rml", .{});
        rml.deinit() catch |err| log.err("on Rml.deinit, {s}", .{@errorName(err)});
    }

    try Rml.bindgen.bindGlobals(rml, rml.main_interpreter.data.evaluation_env, .{
        .@"print-ln" = Rml.procedure.Memory {
            .native = &struct {
                pub fn printLn(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
                    const _rml = interpreter.getRml();

                    const stdout = std.io.getStdOut();
                    const nativeWriter = stdout.writer();

                    const writer: Rml.Writer = try .init(_rml, callOrigin, .{nativeWriter.any()});
                    defer writer.deinit();

                    try writer.data.print("{}: ", .{callOrigin});

                    for (args) |arg| try arg.getHeader().onFormat(writer);

                    try writer.data.writeAll("\n");

                    const nil: Rml.Nil = try .init(_rml, callOrigin);
                    defer nil.deinit();

                    return nil.typeErase();
                }
            }.printLn
        },
        .@"+" = Rml.procedure.Memory {
            .native = &struct {
                pub fn add(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
                    const _rml = interpreter.getRml();
                    if (args.len == 0) return error.InvalidArgumentCount;

                    var sum = args[0].clone();
                    defer sum.deinit();

                    if (args.len == 1) {
                        if (Rml.castObj(Rml.int, sum)) |int| {
                            defer int.deinit();

                            const result: Rml.Int = try .wrap(_rml, callOrigin, @intCast(@abs(int.data.*)));
                            defer result.deinit();

                            return result.typeErase();
                        } else if (Rml.castObj(Rml.float, sum)) |float| {
                            defer float.deinit();

                            const result: Rml.Float = try .wrap(_rml, callOrigin, @abs(float.data.*));
                            defer result.deinit();

                            return result.typeErase();
                        } if (Rml.castObj(Rml.char, sum)) |char| {
                            defer char.deinit();

                            const result: Rml.Char = try .wrap(_rml, callOrigin, char.data.*);
                            defer result.deinit();

                            return result.typeErase();
                        } else {
                            return error.TypeError;
                        }
                    } else {
                        for (args[1..]) |arg| {
                            if (Rml.castObj(Rml.int, sum)) |int| {
                                defer int.deinit();

                                if (Rml.castObj(Rml.int, arg)) |int2| { // int + int = int
                                    defer int2.deinit();

                                    const int3: Rml.Int = try .wrap(_rml, callOrigin, int.data.* + int2.data.*);
                                    defer int3.deinit();

                                    sum.deinit();
                                    sum = int3.typeErase();
                                } else if (Rml.castObj(Rml.float, arg)) |float| { // int + float = float
                                    defer float.deinit();

                                    const float2: Rml.Float = try .wrap(_rml, callOrigin, @as(Rml.float, @floatFromInt(int.data.*)) + float.data.*);
                                    defer float2.deinit();

                                    sum.deinit();
                                    sum = float2.typeErase();
                                } else if (Rml.castObj(Rml.char, arg)) |char| { // int + char = int
                                    defer char.deinit();

                                    const int2: Rml.Int = try .wrap(_rml, callOrigin, int.data.* + @as(Rml.int, @intCast(char.data.*)));
                                    defer int2.deinit();

                                    sum.deinit();
                                    sum = int2.typeErase();
                                } else {
                                    return error.TypeError;
                                }
                            } else if (Rml.castObj(Rml.float, sum)) |float| {
                                defer float.deinit();

                                if (Rml.castObj(Rml.int, arg)) |int| { // float + int = float
                                    defer int.deinit();

                                    const float2: Rml.Float = try .wrap(_rml, callOrigin, float.data.* + @as(Rml.float, @floatFromInt(int.data.*)));
                                    defer float2.deinit();

                                    sum.deinit();
                                    sum = float2.typeErase();
                                } else if (Rml.castObj(Rml.float, arg)) |float2| { // float + float = float
                                    defer float2.deinit();

                                    const float3: Rml.Float = try .wrap(_rml, callOrigin, float.data.* + float2.data.*);
                                    defer float3.deinit();

                                    sum.deinit();
                                    sum = float3.typeErase();
                                } else if (Rml.castObj(Rml.char, arg)) |char| { // float + char = float
                                    defer char.deinit();

                                    const float2: Rml.Float = try .wrap(_rml, callOrigin, float.data.* + @as(Rml.float, @floatFromInt(char.data.*)));
                                    defer float2.deinit();

                                    sum.deinit();
                                    sum = float2.typeErase();
                                } else {
                                    return error.TypeError;
                                }
                            } else if (Rml.castObj(Rml.char, sum)) |char| {
                                defer char.deinit();

                                if (Rml.castObj(Rml.int, arg)) |int| { // char + int = int
                                    defer int.deinit();

                                    const int2: Rml.Int = try .wrap(_rml, callOrigin, @as(Rml.int, @intCast(char.data.*)) + int.data.*);
                                    defer int2.deinit();

                                    sum.deinit();
                                    sum = int2.typeErase();
                                } else if (Rml.castObj(Rml.float, arg)) |float| { // char + float = float
                                    defer float.deinit();

                                    const float2: Rml.Float = try .wrap(_rml, callOrigin, @as(Rml.float, @floatFromInt(char.data.*)) + float.data.*);
                                    defer float2.deinit();

                                    sum.deinit();
                                    sum = float2.typeErase();
                                } else if (Rml.castObj(Rml.char, arg)) |char2| { // char + char = char
                                    defer char2.deinit();

                                    const char3: Rml.Char = try .wrap(_rml, callOrigin, char.data.* + char2.data.*);
                                    defer char3.deinit();

                                    sum.deinit();
                                    sum = char3.typeErase();
                                } else {
                                    return error.TypeError;
                                }
                            } else {
                                return error.TypeError;
                            }
                        }

                        return sum.clone();
                    }
                }
            }.add
        }
    });
    log.info("test start", .{});
    log.info("namespace_env: {}", .{rml.main_interpreter.data.namespace_env});
    log.info("evaluation_env: {}", .{rml.main_interpreter.data.evaluation_env});

    const srcText: []const u8 = "(print-ln \"Hello, world!\" (+ '1' 2))";

    const parser: Rml.Parser = try .init(rml, rml.storage.origin, .{"test.rml", try Rml.String.init(rml, rml.storage.origin, .{srcText})});
    defer {
        log.debug("Deinitializing parser", .{});
        parser.deinit();
    }

    const expr = parser.data.parseDocument() catch |err| {
        log.err("on parseDocument, {s}", .{@errorName(err)});
        return err;
    };
    defer expr.deinit();

    log.info("expr: {}", .{expr});

    const obj = expr.typeErase();
    defer obj.deinit();

    if (rml.main_interpreter.data.eval(obj)) |res| {
        defer res.deinit();

        log.info("result: {}", .{res});
    } else |err| {
        log.err("on eval, {s}", .{@errorName(err)});
    }
}
