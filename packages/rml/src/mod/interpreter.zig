const std = @import("std");
const MiscUtils = @import("Utils").Misc;

const Rml = @import("root.zig");
const Ordering = Rml.Ordering;
const Error = Rml.Error;
const OOM = Rml.OOM;
const log = Rml.log;
const Env = Rml.Env;
const Symbol = Rml.Symbol;
const Writer = Rml.Writer;
const Object = Rml.Object;
const Origin = Rml.Origin;
const Obj = Rml.Obj;
const ptr = Rml.ptr;
const getObj = Rml.getObj;
const getTypeId = Rml.getTypeId;
const getRml = Rml.getRml;
const castObj = Rml.castObj;
const forceObj = Rml.forceObj;


pub const evaluation = std.log.scoped(.evaluation);


pub const Interpreter = Obj(Memory);

pub const Result = Signal || Error;
pub const Signal = error { Terminate };
pub const EvalError = error {
    TypeError,
    UnboundSymbol,
    InvalidArgumentCount,
};

pub const Memory = struct {
    namespace_env: Env,
    evaluation_env: Env,

    pub fn onInit(self: ptr(Memory), namespace_env: Env, evaluation_env: Env) OOM! void {
        evaluation.debug("initializing Interpreter{x}", .{@intFromPtr(self)});
        self.namespace_env = namespace_env;
        self.evaluation_env = evaluation_env;
    }

    pub fn onCompare(a: ptr(Memory), other: Object) Ordering {
        return Rml.compare(@intFromPtr(a), @intFromPtr(other.data));
    }

    pub fn onFormat(self: ptr(Memory), writer: Writer) Error! void {
        return writer.data.print("Interpreter{x}", .{@intFromPtr(self)});
    }

    pub fn onDeinit(self: ptr(Memory)) void {
        evaluation.debug("deinitializing Interpreter{x}", .{@intFromPtr(self)});
        self.namespace_env.deinit();
        self.evaluation_env.deinit();
    }

    pub fn eval(self: ptr(Memory), expr: Object) Result! Object {
        return self.evalCheck(expr, null);
    }

    pub fn evalAll(self: ptr(Memory), exprs: []const Object) Result! Rml.array.ObjectMemoryUnmanaged {
        return self.evalAllCheck(exprs, null);
    }

    pub fn evalAllCheck(self: ptr(Memory), exprs: []const Object, workDone: ?*bool) Result! Rml.array.ObjectMemoryUnmanaged {
        const rml = getRml(self);

        var arr: Rml.array.ObjectMemoryUnmanaged = .{};
        errdefer arr.deinit(rml);

        for (exprs) |expr| {
            const result = try self.evalCheck(expr, workDone);
            try arr.append(rml, result);
        }

        return arr;
    }

    pub fn evalCheck(self: ptr(Memory), expr: Object, workDone: ?*bool) Result! Object {
        const exprTypeId = expr.getHeader().type_id;

        if (Rml.equal(exprTypeId, Rml.TypeId.of(Rml.block.Memory))) {
            if (workDone) |x| x.* = true;

            const block = forceObj(Rml.block.Memory, expr);
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
        } else if (Rml.equal(exprTypeId, Rml.TypeId.of(Rml.symbol.Memory))) {
            if (workDone) |x| x.* = true;

            const symbol = forceObj(Rml.symbol.Memory, expr);
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

    pub fn lookupNamespace(self: ptr(Memory), symbol: Symbol) ?Object {
        return self.namespace_env.data.get(symbol);
    }

    pub fn lookup(self: ptr(Memory), symbol: Symbol) ?Object {
        return self.evaluation_env.data.get(symbol);
    }

    pub fn runProgram(self: ptr(Memory), program: Rml.Block) Result! Object {
        const rml = getRml(self);

        const nil: Rml.Nil = try .init(rml, program.getHeader().origin);
        var result: Object = nil.typeErase();
        nil.deinit();
        errdefer result.deinit();

        const exprs = program.data.array.items();

        for (exprs) |expr| {
            const result_obj = try self.eval(expr);

            result.deinit();
            result = result_obj;
        }

        return result;
    }

    pub fn invoke(self: ptr(Memory), callOrigin: Origin, function: Object, args: []const Object) Result! Object {
        const functionTypeId = function.getHeader().type_id;

        if (Rml.equal(functionTypeId, Rml.TypeId.of(Rml.procedure.Memory))) {
            const procedure = forceObj(Rml.procedure.Memory, function);
            defer procedure.deinit();

            switch (procedure.data.*) {
                .macro => { unreachable; },
                .function => { unreachable; },
                .native => |func| {
                    var eArgs = try self.evalAll(args);
                    defer eArgs.deinit(getRml(self));

                    const selfObj = getObj(self);
                    defer selfObj.deinit();

                    return func(selfObj, callOrigin, eArgs.items());
                },
            }
        } else {
            return error.TypeError;
        }
    }
};

