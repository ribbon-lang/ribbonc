const std = @import("std");

const Rml = @import("root.zig");
const Error = Rml.Error;
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const const_ptr = Rml.const_ptr;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const Object = Rml.Object;
const Block = Rml.Block;
const Pattern = Rml.Pattern;
const Writer = Rml.Writer;
const getHeader = Rml.getHeader;
const getObj = Rml.getObj;
const getRml = Rml.getRml;
const forceObj = Rml.forceObj;

pub const ProcedureKind = enum {
    macro,
    function,
    native_macro,
    native_function,
};

pub const Case = union(enum) {
    @"else": Rml.array.ArrayUnmanaged,

    pattern: struct {
        scrutinizer: Obj(Pattern),
        body: Rml.array.ArrayUnmanaged,
    },

    pub fn body(self: ptr(Case)) *Rml.array.ArrayUnmanaged {
        return switch (self.*) {
            .@"else" => |*arr| arr,
            .pattern => |*data| &data.body,
        };
    }

    pub fn onDeinit(self: ptr(Case)) void {
        switch (self.*) {
            .@"else" => |*arr| arr.deinit(getRml(self)),
            .pattern => |*data| {
                data.scrutinizer.deinit();
                data.body.deinit(getRml(self));
            },
        }
    }
};

pub const ProcedureBody = struct {
    cases: Rml.array.TypedArrayUnmanaged(Case),

    pub fn deinit(self: *ProcedureBody, rml: *Rml) void {
        self.cases.deinit(rml);
    }
};

pub const Procedure = union(ProcedureKind) {
    macro: ProcedureBody,
    function: ProcedureBody,
    native_macro: Rml.bindgen.NativeFunction,
    native_function: Rml.bindgen.NativeFunction,

    pub fn onInit(_: ptr(Procedure)) OOM! void {
        return;
    }

    pub fn onCompare(self: ptr(Procedure), other: Object) Ordering {
        return Rml.compare(getHeader(self).type_id, other.getTypeId());
    }

    pub fn onFormat(self: ptr(Procedure), writer: Rml.Obj(Writer)) Error! void {
        return writer.data.print("[{s}-{x}]", .{@tagName(self.*), @intFromPtr(self)});
    }

    pub fn onDeinit(self: ptr(Procedure)) void {
        switch (self.*) {
            .macro => |*data| data.deinit(getRml(self)),
            .function => |*data| data.deinit(getRml(self)),
            .native_macro => {},
            .native_function => {},
        }
    }

    pub fn call(self: ptr(Procedure), interpreter: ptr(Rml.Interpreter), callOrigin: Rml.Origin, blame: Object, args: []const Object) Rml.Result! Object {
        switch (self.*) {
            .macro => { unreachable; },
            .function => |func| {
                var eArgs = try interpreter.evalAll(args);
                defer eArgs.deinit(getRml(self));

                var errors: Rml.string.StringUnmanaged = .{};
                defer errors.deinit(getRml(self));

                const writer = errors.writer(getRml(self));

                for (func.cases.items()) |case| switch (case.data.*) {
                    .@"else" => |caseData| {
                        return interpreter.runProgram(case.getOrigin(), caseData.items());
                    },
                    .pattern => |caseData| {
                        var diag: ?Rml.Diagnostic = null;
                        const result: ?Obj(Rml.map.Table) = try caseData.scrutinizer.data.run(interpreter, &diag, callOrigin, eArgs.items());
                        if (result) |res| {
                            defer res.deinit();

                            const oldEnv = interpreter.evaluation_env;
                            defer {
                                interpreter.evaluation_env.deinit();
                                interpreter.evaluation_env = oldEnv;
                            }

                            interpreter.evaluation_env = try Obj(Rml.Env).wrap(getRml(self), callOrigin, Rml.Env {
                                .parent = Rml.downgradeCast(oldEnv),
                                .table = try res.data.unmanaged.clone(getRml(self)),
                            });

                            return interpreter.runProgram(case.getOrigin(), caseData.body.items());
                        } else if (diag) |d| {
                            writer.print("failed to match; {} vs {}:\n\t{}", .{ caseData.scrutinizer, eArgs, d.formatter(error.PatternError)})
                                catch |err| return Rml.errorCast(err);
                        } else {
                            Rml.interpreter.evaluation.err("requested pattern diagnostic is null", .{});
                            writer.print("failed to match; {} vs {}", .{ caseData.scrutinizer, eArgs})
                                catch |err| return Rml.errorCast(err);
                        }
                    },
                };

                try interpreter.abort(callOrigin, error.PatternError, "{} failed; no matching case found for input", .{blame});
            },
            .native_macro => |func| return func(interpreter, callOrigin, args),
            .native_function => |func| {
                var eArgs = try interpreter.evalAll(args);
                defer eArgs.deinit(getRml(self));

                return func(interpreter, callOrigin, eArgs.items());
            },
        }
    }
};
