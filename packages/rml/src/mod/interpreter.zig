const std = @import("std");
const MiscUtils = @import("Utils").Misc;

const Rml = @import("root.zig");
const Ordering = Rml.Ordering;
const Error = Rml.Error;
const OOM = Rml.OOM;
const log = Rml.log;
const Object = Rml.Object;
const Origin = Rml.Origin;
const Obj = Rml.Obj;
const ptr = Rml.ptr;
const Nil = Rml.Nil;
const Env = Rml.Env;
const Symbol = Rml.Symbol;
const Writer = Rml.Writer;
const getObj = Rml.getObj;
const getTypeId = Rml.getTypeId;
const getRml = Rml.getRml;
const castObj = Rml.castObj;
const forceObj = Rml.forceObj;


pub const evaluation = std.log.scoped(.evaluation);


pub const Result = Signal || Error;
pub const Signal = error { Terminate };
pub const EvalError = error {
    TypeError,
    UnboundSymbol,
    InvalidArgumentCount,
};
pub const Interpreter = struct {
    namespace_env: Obj(Env),
    evaluation_env: Obj(Env),

    pub fn onInit(self: ptr(Interpreter), namespace_env: Obj(Env), evaluation_env: Obj(Env)) OOM! void {
        evaluation.debug("initializing Obj(Interpreter){x}", .{@intFromPtr(self)});
        self.namespace_env = namespace_env;
        self.evaluation_env = evaluation_env;
    }

    pub fn onCompare(a: ptr(Interpreter), other: Object) Ordering {
        return Rml.compare(@intFromPtr(a), @intFromPtr(other.data));
    }

    pub fn onFormat(self: ptr(Interpreter), writer: Obj(Writer)) Error! void {
        return writer.data.print("Obj(Interpreter){x}", .{@intFromPtr(self)});
    }

    pub fn onDeinit(self: ptr(Interpreter)) void {
        evaluation.debug("deinitializing Obj(Interpreter){x}", .{@intFromPtr(self)});
        self.namespace_env.deinit();
        self.evaluation_env.deinit();
    }

    pub fn abort(self: ptr(Interpreter), origin: Origin, err: Error, comptime fmt: []const u8, args: anytype) Error! noreturn {
        const diagnostic = getRml(self).diagnostic orelse return err;

        var diag = Rml.Diagnostic {
            .err = err,
            .error_origin = origin,
        };

        // the error produced is only NoSpaceLeft, if the buffer is too small, so give the length of the buffer
        diag.message_len = len: {
            break :len (std.fmt.bufPrintZ(&diag.message_mem, fmt, args) catch {
                log.warn("Diagnostic message too long, truncating", .{});
                break :len Rml.Diagnostic.MAX_LENGTH;
            }).len;
        };

        diagnostic.* = diag;

        return err;
    }

    pub fn eval(self: ptr(Interpreter), expr: Object) Result! Object {
        return self.evalCheck(expr, null);
    }

    pub fn evalAll(self: ptr(Interpreter), exprs: []const Object) Result! Rml.array.ArrayUnmanaged {
        return self.evalAllCheck(exprs, null);
    }

    pub fn evalAllCheck(self: ptr(Interpreter), exprs: []const Object, workDone: ?*bool) Result! Rml.array.ArrayUnmanaged {
        const rml = getRml(self);

        var arr: Rml.array.ArrayUnmanaged = .{};
        errdefer arr.deinit(rml);

        for (exprs) |expr| {
            const result = try self.evalCheck(expr, workDone);
            try arr.append(rml, result);
        }

        return arr;
    }

    pub fn evalCheck(self: ptr(Interpreter), expr: Object, workDone: ?*bool) Result! Object {
        const exprTypeId = expr.getHeader().type_id;

        if (Rml.equal(exprTypeId, Rml.TypeId.of(Rml.block.Block))) {
            if (workDone) |x| x.* = true;

            const block = forceObj(Rml.block.Block, expr);
            defer block.deinit();

            switch (block.data.block_kind) {
                .doc => {
                    evaluation.debug("running doc", .{});
                    return self.runProgram(block);
                },
                .paren => {
                    const items = block.data.array.items();
                    evaluation.debug("performing call {any}", .{items});
                    const function = items[0];
                    const args = items[1..];

                    const function_obj = try self.eval(function);
                    defer function_obj.deinit();

                    return self.invoke(block.getHeader().origin, function_obj, args);
                },
                else => return error.TypeError,
            }
        } else if (Rml.equal(exprTypeId, Rml.TypeId.of(Rml.symbol.Symbol))) {
            if (workDone) |x| x.* = true;

            const symbol = forceObj(Rml.symbol.Symbol, expr);
            defer symbol.deinit();

            evaluation.debug("looking up symbol {}", .{symbol});

            return self.lookup(symbol) orelse {
                evaluation.err("unbound symbol {}", .{symbol});
                return error.UnboundSymbol;
            };
        }

        evaluation.debug("cannot evaluate further", .{});

        return expr.clone();
    }

    pub fn lookupNamespace(self: ptr(Interpreter), symbol: Obj(Symbol)) ?Object {
        return self.namespace_env.data.get(symbol);
    }

    pub fn lookup(self: ptr(Interpreter), symbol: Obj(Symbol)) ?Object {
        return self.evaluation_env.data.get(symbol);
    }

    pub fn runProgram(self: ptr(Interpreter), program: Obj(Rml.Block)) Result! Object {
        const rml = getRml(self);

        var result: Object = (try Obj(Nil).init(rml, program.getHeader().origin)).typeEraseLeak();
        errdefer result.deinit();

        const exprs = program.data.array.items();

        for (exprs) |expr| {
            const result_obj = try self.eval(expr);

            result.deinit();
            result = result_obj;
        }

        return result;
    }

    pub fn invoke(self: ptr(Interpreter), callOrigin: Origin, function: Object, args: []const Object) Result! Object {
        const functionTypeId = function.getHeader().type_id;

        if (Rml.equal(functionTypeId, Rml.TypeId.of(Rml.procedure.Procedure))) {
            const procedure = forceObj(Rml.procedure.Procedure, function);
            defer procedure.deinit();

            switch (procedure.data.*) {
                .macro => { unreachable; },
                .function => { unreachable; },
                .native_macro => |func| return func(self, callOrigin, args),
                .native_function => |func| {
                    var eArgs = try self.evalAll(args);
                    defer eArgs.deinit(getRml(self));

                    return func(self, callOrigin, eArgs.items());
                },
            }
        } else {
            try self.abort(callOrigin, error.TypeError, "expected a procedure, got {s}: {s}", .{Rml.TypeId.name(function.getHeader().type_id), function});
        }
    }
};

