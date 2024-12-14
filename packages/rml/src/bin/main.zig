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

    var diagnostic: ?Rml.Diagnostic = null;

    const rml: *Rml = try .init(gpa.allocator(), null, null, &diagnostic, &.{});
    log.debug("rml initialized", .{});
    defer {
        log.debug("Deinitializing Rml", .{});
        rml.deinit() catch |err| log.err("on Rml.deinit, {s}", .{@errorName(err)});
    }

    log.info("test start", .{});


    Rml.object.refcount.debug("test start object_count: {}", .{rml.storage.object_count});
    defer Rml.object.refcount.debug("test end object_count: {}", .{rml.storage.object_count});

    log.debug("namespace_env: {}", .{rml.namespace_env});
    log.debug("global_env: {}", .{rml.global_env});
    log.debug("evaluation_env: {}", .{rml.main_interpreter.data.evaluation_env});


    const srcText: []const u8 = try std.fs.cwd().readFileAlloc(rml.storage.object, "test.bb", std.math.maxInt(u16));
    defer rml.storage.object.free(srcText);

    const parser: Rml.Obj(Rml.Parser) = try .init(rml, rml.storage.origin, .{"test.bb", try Rml.Obj(Rml.String).init(rml, rml.storage.origin, .{srcText})});
    defer {
        log.debug("Deinitializing parser", .{});
        parser.deinit();
    }

    const input: Rml.Obj(Rml.Int) = try .wrap(rml, rml.storage.origin, 10);
    defer {
        log.debug("Deinitializing input", .{});
        input.deinit();
    }

    var lineMem: Rml.array.ArrayUnmanaged = .{};
    defer lineMem.deinit(rml);

    while (!parser.data.isEof()) {
        const startPos = parser.data.pos;

        const start = parser.data.peek() catch |err| {
            if (diagnostic) |diag| {
                log.err("{s} {}: {s}", .{@errorName(err), diag.error_origin, diag.message_mem[0..diag.message_len]});
            } else {
                log.err("requested parser diagnostic is null", .{});
            }
            return err;
        } orelse @panic("not eof, but peek got null");
        defer start.deinit();

        log.debug("gathering line", .{});
        const line =
            line: while (next: {
                log.debug("getting next sourceExpr", .{});
                const next = parser.data.next() catch |err| {
                    if (diagnostic) |diag| {
                        log.err("{s} {}: {s}", .{@errorName(err), diag.error_origin, diag.message_mem[0..diag.message_len]});
                    } else {
                        log.err("requested parser diagnostic is null", .{});
                    }
                    return err;
                };
                log.debug("next: {?}", .{next});
                break :next next;
            }) |sourceExpr| {
                log.debug("got sourceExpr {}", .{sourceExpr});

                const next: ?Rml.Object = parser.data.peek() catch |err| {
                    if (diagnostic) |diag| {
                        log.err("{s} {}: {s}", .{@errorName(err), diag.error_origin, diag.message_mem[0..diag.message_len]});
                    } else {
                        log.err("requested parser diagnostic is null", .{});
                    }
                    return err;
                };
                defer if (next) |x| x.deinit();

                try lineMem.append(rml, sourceExpr);
                log.debug("added to lineMem", .{});

                var nxt = next orelse {
                    log.debug("next is null; break", .{});
                    break :line lineMem.items();
                };

                log.debug("next: {}", .{nxt});

                const startRange = start.getHeader().origin.range.?;
                const nxtRange = nxt.getHeader().origin.range.?;

                log.debug("startRange: {}, nxtRange: {}", .{startRange, nxtRange});

                if (nxtRange.start.?.line > startRange.end.?.line) {
                    if (nxtRange.start.?.column <= startRange.start.?.column) {
                        log.debug("break!", .{});
                        break :line lineMem.items();
                    }
                }

                log.debug("continue!", .{});
            } else {
                @panic("peek not null but next got null?");
            };
        defer {
            log.debug("clearing lineMem", .{});
            lineMem.clear(rml);
        }

        const lineOrigin = parser.data.getOrigin(startPos, parser.data.pos);

        log.info("line {}: {any}", .{lineOrigin, line});

        if (rml.main_interpreter.data.runProgram(lineOrigin, line)) |result| {
            log.debug("i'm not dead i swear", .{});
            defer result.deinit();

            log.info("result: {}", .{result});
        } else |err| {
            log.err("on eval, {s}", .{@errorName(err)});
            if (diagnostic) |diag| {
                log.err("{s} {}: {s}", .{@errorName(err), diag.error_origin, diag.message_mem[0..diag.message_len]});
            } else {
                log.err("requested interpreter diagnostic is null", .{});
            }

            diagnostic = null;

            return err;
        }
    }
}
