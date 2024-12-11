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
    log.debug("namespace_env: {}", .{rml.main_interpreter.data.namespace_env});
    log.debug("evaluation_env: {}", .{rml.main_interpreter.data.evaluation_env});

    const srcText: []const u8 = "(print-ln \"Hello, world!\" (or nil 10))";

    const parser: Rml.Obj(Rml.Parser) = try .init(rml, rml.storage.origin, .{"test.rml", try Rml.Obj(Rml.String).init(rml, rml.storage.origin, .{srcText})});
    defer {
        log.debug("Deinitializing parser", .{});
        parser.deinit();
    }

    const expr = parser.data.parseDocument() catch |err| {
        log.err("on parseDocument, {s}", .{@errorName(err)});
        if (diagnostic) |diag| {
            log.err("{s} {}: {s}", .{@errorName(diag.err), diag.error_origin, diag.message_mem[0..diag.message_len]});
        } else {
            log.err("requested diagnostic is null", .{});
        }
        return err;
    };
    defer expr.deinit();

    log.info("expr: {}", .{expr});

    if (rml.main_interpreter.data.runProgram(expr)) |res| {
        defer res.deinit();

        log.info("result: {}", .{res});
    } else |err| {
        log.err("on eval, {s}", .{@errorName(err)});
        if (diagnostic) |diag| {
            log.err("{s} {}: {s}", .{@errorName(diag.err), diag.error_origin, diag.message_mem[0..diag.message_len]});
        } else {
            log.err("requested diagnostic is null", .{});
        }
    }
}
