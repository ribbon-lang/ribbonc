const std = @import("std");

const MiscUtils = @import("Utils").Misc;
const TextUtils = @import("Utils").Text;
const TypeUtils = @import("Utils").Type;
const Config = @import("Config");
const Builtin = @import("Builtin");

const Core = @import("root.zig");
const SExpr = Core.SExpr;
const Context = Core.Context;
const Source = Core.Source;

context: *Context,
errorCause: ?[]const u8,
attr: ?*const Source.Attr,
env: SExpr,
callerEnv: SExpr,
evidence: SExpr,
globalEvidence: SExpr,
callDepth: usize = 0,
terminationData: ?TerminationData = null,

const TerminationData = struct {
    ctxId: SExpr,
    value: SExpr,
};

const Interpreter = @This();

pub const Result = Signal || Error;
pub const Signal = error{Terminate};
pub const Error = TextUtils.Error || Context.Error || EvaluationError;
pub const EvaluationError = error{
    Panic,
    NotEvaluatable,
    NotCallable,
    TypeError,
    RangeError,
    NotEnoughArguments,
    TooManyArguments,
    DivisionByZero,
    UnboundSymbol,
    InvalidContext,
    EnvironmentUnderflow,
    CallStackOverflow,
    MissingDynamic,
    UnexpectedTerminate,
    MissingTerminationData,
};

pub fn asResult(r: anyerror) ?Result {
    return TypeUtils.narrowErrorSet(Result, r);
}

pub fn asSignal(r: anyerror) ?Signal {
    return TypeUtils.narrowErrorSet(Signal, r);
}

pub fn asEvaluationError(e: anyerror) ?EvaluationError {
    return TypeUtils.narrowErrorSet(EvaluationError, e);
}

pub fn asError(e: anyerror) ?Error {
    return TypeUtils.narrowErrorSet(Error, e);
}

pub fn isResult(r: anyerror) bool {
    return TypeUtils.isInErrorSet(Result, r);
}

pub fn isSignal(r: anyerror) bool {
    return TypeUtils.isInErrorSet(Signal, r);
}

pub fn isEvaluationError(e: anyerror) bool {
    return TypeUtils.isInErrorSet(EvaluationError, e);
}

pub fn isError(e: anyerror) bool {
    return TypeUtils.isInErrorSet(Error, e);
}

pub const ExternMessage = enum(u8) {
    SigTerminate,

    ErrPanic,
    ErrNotEvaluatable,
    ErrNotCallable,
    ErrTypeError,
    ErrRangeError,
    ErrNotEnoughArguments,
    ErrTooManyArguments,
    ErrDivisionByZero,
    ErrUnboundSymbol,
    ErrInvalidContext,
    ErrEnvironmentUnderflow,
    ErrCallStackOverflow,
    ErrMissingDynamic,
    ErrUnexpectedTerminate,
    ErrMissingTerminationData,

    ErrOutOfMemory,
    ErrBadEncoding,
};

pub const RichError = struct {
    err: Error,
    msg: ?[]const u8,
    attr: ?*const Source.Attr,

    const Self = @This();

    pub fn initFromInterpreter(interpreter: *const Interpreter, err: Error) Self {
        return Self{ .err = err, .msg = interpreter.errorCause, .attr = interpreter.attr };
    }

    pub fn format(self: *const Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Evaluation error", .{});
        if (self.attr) |attr| {
            try writer.print(" at {}", .{attr});
        }
        try writer.print(": ", .{});

        try self.printError(writer);

        if (self.msg) |cause| {
            try writer.print("\n\t{s}", .{cause});
        }
    }

    fn printError(self: *const Self, writer: anytype) !void {
        switch (self.err) {
            EvaluationError.Panic => return writer.print("Panic", .{}),
            EvaluationError.NotEvaluatable => return writer.print("Expression is not evaluatable", .{}),
            EvaluationError.NotCallable => return writer.print("Expression is not callable", .{}),
            EvaluationError.TypeError => return writer.print("Type error", .{}),
            EvaluationError.RangeError => return writer.print("Range error", .{}),
            EvaluationError.NotEnoughArguments => return writer.print("Not enough arguments", .{}),
            EvaluationError.TooManyArguments => return writer.print("Too many arguments", .{}),
            EvaluationError.DivisionByZero => return writer.print("Division by zero", .{}),
            EvaluationError.UnboundSymbol => return writer.print("Unbound symbol", .{}),
            EvaluationError.InvalidContext => return writer.print("Invalid context", .{}),
            EvaluationError.EnvironmentUnderflow => return writer.print("Environment underflow (no frame to pop)", .{}),
            EvaluationError.CallStackOverflow => return writer.print("Call stack overflow (max call depth is {})", .{Config.MAX_DEPTH}),
            EvaluationError.MissingDynamic => return writer.print("Missing dynamic binding", .{}),
            EvaluationError.UnexpectedTerminate => return writer.print("Unexpected `terminate`", .{}),
            EvaluationError.MissingTerminationData => return writer.print("Missing termination data", .{}),

            TextUtils.Error.BadEncoding => return writer.print("Bad text encoding", .{}),
            Context.Error.OutOfMemory => return writer.print("Out of memory", .{}),
        }
    }
};

pub fn init(context: *Context) Error!*Interpreter {
    const nil = try SExpr.Nil(context.attr);

    const ptr = try context.allocator.create(Interpreter);

    ptr.* = Interpreter{
        .context = context,
        .errorCause = null,
        .attr = null,
        .env = try SExpr.List(context.attr, &[1]SExpr{try SExpr.List(context.attr, &[0]SExpr{})}),
        .callerEnv = nil,
        .evidence = try SExpr.List(context.attr, &[1]SExpr{try SExpr.List(context.attr, &[0]SExpr{})}),
        .globalEvidence = nil,
    };

    return ptr;
}

pub fn deinit(interpreter: *Interpreter) void {
    if (interpreter.errorCause) |cause| {
        interpreter.context.allocator.free(cause);
    }
    interpreter.context.allocator.destroy(interpreter);
}

pub const SavedEvaluationEnvs = struct { SExpr, SExpr };

pub fn save(interpreter: *const Interpreter) !SavedEvaluationEnvs {
    return .{ try copyEnv(interpreter.context.attr, interpreter.env), try copyEnv(interpreter.context.attr, interpreter.callerEnv) };
}

pub fn restore(interpreter: *Interpreter, envs: SavedEvaluationEnvs) void {
    interpreter.env = envs[0];
    interpreter.callerEnv = envs[1];

    if (interpreter.errorCause) |cause| {
        interpreter.context.allocator.free(cause);
        interpreter.errorCause = null;
    }

    interpreter.attr = null;

    interpreter.callDepth = 0;
}

pub fn exit(interpreter: *Interpreter, err: Error, attr: *const Source.Attr) Result {
    interpreter.attr = attr;
    interpreter.errorCause = null;
    return err;
}

pub fn abort(interpreter: *Interpreter, err: Error, attr: *const Source.Attr, comptime fmt: []const u8, args: anytype) Result {
    interpreter.attr = attr;
    interpreter.errorCause = try std.fmt.allocPrint(interpreter.context.allocator, fmt, args);
    return err;
}

pub fn errDiagnosticFilled(interpreter: *const Interpreter) bool {
    return interpreter.errorCause != null;
}

pub fn errFmt(interpreter: *const Interpreter, err: Error) RichError {
    return RichError.initFromInterpreter(interpreter, err);
}

pub fn envLookupPair(symbol: SExpr, env: SExpr) Error!?SExpr {
    var current = env;
    while (!current.isNil()) {
        const xp = current.castCons() orelse return EvaluationError.TypeError;
        const frame = xp.car;
        const rest = xp.cdr;

        if (try frameLookup(symbol, frame)) |pair| {
            return pair;
        }

        current = rest;
    }

    return null;
}

pub fn envLookup(symbol: SExpr, env: SExpr) Error!?SExpr {
    const pair = try envLookupPair(symbol, env) orelse return null;

    if (pair.castCons()) |xp| {
        return xp.cdr;
    } else {
        return EvaluationError.TypeError;
    }
}

pub fn envKeys(env: SExpr, allocator: std.mem.Allocator) Error![]SExpr {
    var keyset = SExpr.HashSet.init(allocator);
    defer keyset.deinit();

    try envKeysIn(env, &keyset);

    return try allocator.dupe(SExpr, keyset.keys());
}

pub fn envKeyStrs(env: SExpr, allocator: std.mem.Allocator) Error![]const []const u8 {
    var keyset = SExpr.HashSet.init(allocator);
    defer keyset.deinit();

    try envKeysIn(env, &keyset);

    var buf = std.ArrayList([]const u8).init(allocator);
    defer buf.deinit();

    for (keyset.keys()) |key| {
        try buf.append(key.forceSymbolSlice());
    }

    return try buf.toOwnedSlice();
}

pub fn envKeysIn(env: SExpr, keyset: *SExpr.HashSet) Error!void {
    var current = env;
    while (!current.isNil()) {
        const xp = current.castCons() orelse return EvaluationError.TypeError;

        try frameKeysIn(xp.car, keyset);

        current = xp.cdr;
    }
}

pub fn frameKeys(frame: SExpr) Error![]SExpr {
    const allocator = frame.getAttr().context.allocator;

    var keyset = SExpr.HashSet.init(allocator);
    defer keyset.deinit();

    try frameKeysIn(frame, &keyset);

    return allocator.dupe(SExpr, keyset.keys());
}

pub fn frameKeysIn(frame: SExpr, keyset: *SExpr.HashSet) Error!void {
    var current = frame;
    while (!current.isNil()) {
        const xp = current.castCons() orelse return EvaluationError.TypeError;
        const binding = xp.car;
        const rest = xp.cdr;

        const bindingXp = binding.castCons() orelse return EvaluationError.TypeError;

        if (bindingXp.car.isSymbol()) {
            try keyset.put(bindingXp.car, {});
        }

        current = rest;
    }
}

pub fn frameLookup(symbol: SExpr, frame: SExpr) Error!?SExpr {
    var current = frame;

    while (!current.isNil()) {
        const xp = current.castCons() orelse return EvaluationError.TypeError;
        const binding = xp.car;
        const rest = xp.cdr;

        const bindingXp = binding.castCons() orelse {
            Core.log.err("frameLookup: expected binding to be a cons, got {}: `{}` in frame `{}`", .{ binding.getTag(), binding, frame });
            return EvaluationError.TypeError;
        };

        if (MiscUtils.equal(bindingXp.car, symbol)) {
            return binding;
        }

        current = rest;
    }

    return null;
}

pub fn copyEnv(at: *const Source.Attr, env: SExpr) Error!SExpr {
    var newEnv = try SExpr.Nil(at);
    var current = env;

    while (!current.isNil()) {
        const xp = current.castCons() orelse return EvaluationError.TypeError;
        const frame = xp.car;
        const rest = xp.cdr;

        const newFrame = try copyFrame(at, frame);

        newEnv = try SExpr.Cons(at, newFrame, newEnv);

        current = rest;
    }

    return newEnv;
}

pub fn copyFrame(at: *const Source.Attr, frame: SExpr) Error!SExpr {
    var current = frame;
    var newFrame = try SExpr.Nil(at);

    while (!current.isNil()) {
        const xp = current.castCons() orelse return EvaluationError.TypeError;
        const binding = xp.car;
        const rest = xp.cdr;

        const newBinding = try copyBinding(at, binding);

        newFrame = try SExpr.Cons(at, newBinding, newFrame);

        current = rest;
    }

    return newFrame;
}

pub fn copyBinding(at: *const Source.Attr, binding: SExpr) Error!SExpr {
    const bindingXp = binding.castCons() orelse return EvaluationError.TypeError;
    return try SExpr.Cons(at, bindingXp.car, bindingXp.cdr);
}

pub fn validateEnv(env: SExpr) Error!void {
    var current = env;

    while (!current.isNil()) {
        const xp = current.castCons() orelse return EvaluationError.TypeError;
        const frame = xp.car;
        const rest = xp.cdr;

        try validateFrame(frame);

        current = rest;
    }
}

pub fn validateFrame(frame: SExpr) Error!void {
    var current = frame;

    while (!current.isNil()) {
        const xp = current.castCons() orelse return EvaluationError.TypeError;
        const binding = xp.car;
        const rest = xp.cdr;

        const bindingXp = binding.castCons() orelse return EvaluationError.TypeError;

        if (!bindingXp.car.isSymbol()) {
            return EvaluationError.TypeError;
        }

        current = rest;
    }
}

pub fn pushFrame(frame: SExpr, env: *SExpr) Error!void {
    const newEnv = try SExpr.Cons(frame.getAttr(), frame, env.*);
    env.* = newEnv;
}

pub fn popFrame(env: *SExpr) Error!SExpr {
    if (env.castCons()) |xp| {
        env.* = xp.cdr;
        return xp.car;
    } else if (env.isNil()) {
        return EvaluationError.EnvironmentUnderflow;
    } else {
        return EvaluationError.TypeError;
    }
}

pub fn getFrame(frameOffset: usize, env: SExpr) Error!SExpr {
    var current = env;
    for (0..frameOffset) |_| {
        if (current.isNil()) {
            return EvaluationError.EnvironmentUnderflow;
        }

        const xp = current.castCons() orelse return EvaluationError.TypeError;
        current = xp.cdr;
    }

    return (current.castCons() orelse return EvaluationError.EnvironmentUnderflow).car;
}

pub fn pushNewFrame(at: *const Source.Attr, env: *SExpr) Error!void {
    try pushFrame(try SExpr.Nil(at), env);
}

pub fn extendEnvFrame(at: *const Source.Attr, symbol: SExpr, value: SExpr, env: SExpr) Error!void {
    const frame = (env.castCons() orelse return EvaluationError.TypeError).car;

    const pair = try SExpr.Cons(at, symbol, value);

    const frameExt = try SExpr.Cons(at, pair, frame);

    env.forceCons().car = frameExt;
}

pub fn extendFrame(at: *const Source.Attr, symbol: SExpr, value: SExpr, frame: *SExpr) Error!void {
    const pair = try SExpr.Cons(at, symbol, value);
    frame.* = try SExpr.Cons(at, pair, frame.*);
}

pub fn nativeFetch(interpreter: *Interpreter, at: *const Source.Attr, prompt: []const u8) Result!SExpr {
    const symbol = try SExpr.Symbol(at, prompt);
    return liftFetch(interpreter, at, symbol);
}

pub fn nativePrompt(interpreter: *Interpreter, at: *const Source.Attr, prompt: []const u8, args: anytype) Result!SExpr {
    const symbol = try SExpr.Symbol(at, prompt);
    const argsList = SExpr.MappedList(at, args, SExpr.Quote) catch |err| {
        return interpreter.abort(err, at,
            "failed to map argument list:" ++ switch (@TypeOf(args)) {SExpr => "`{}`", else => "`{any}`"},
            .{args});
    };
    return liftPrompt(interpreter, at, symbol, argsList);
}

pub fn nativeInvoke(interpreter: *Interpreter, at: *const Source.Attr, callback: SExpr, args: anytype) Result!SExpr {
    const argsList = SExpr.MappedList(at, args, SExpr.Quote) catch |err| {
        return interpreter.abort(err, at,
            "failed to map argument list:" ++ switch (@TypeOf(args)) {SExpr => "`{}`", else => "`{any}`"},
            .{args});
    };
    return invoke(interpreter, at, callback, argsList);
}

pub const NativeWithOut = union(enum) { Evaluated: SExpr, Terminated: SExpr };

pub fn nativeWith(
    interpreter: *Interpreter, at: *const Source.Attr,
    prompt: []const u8,
    handler: SExpr.Types.Builtin.Proc,
    body: SExpr,
    out: *NativeWithOut,
) Result!void {
    const baseEv = interpreter.evidence;
    try pushNewFrame(at, &interpreter.evidence);
    defer interpreter.evidence = baseEv;

    const promptSym = try SExpr.Symbol(at, prompt);
    const contextId = try SExpr.Int(at, @intCast(interpreter.context.genId()));
    const wrappedHandler = try wrapNativeHandler(interpreter, at, contextId, promptSym, handler);

    try extendEnvFrame(at, promptSym, wrappedHandler, interpreter.evidence);

    const value = interpreter.eval(body) catch |res| {
        if (res == Signal.Terminate) {
            const terminationData = interpreter.terminationData orelse {
                return EvaluationError.MissingTerminationData;
            };
            if (MiscUtils.equal(terminationData.ctxId, contextId)) {
                out.* = .{ .Terminated = terminationData.value };
                interpreter.terminationData = null;
                return;
            }
        }
        return res;
    };

    out.* = .{ .Evaluated = value };
}

fn wrapNativeHandler(interpreter: *Interpreter, at: *const Source.Attr, ctxId: SExpr, promptSym: SExpr, handler: SExpr.Types.Builtin.Proc) Result!SExpr {
    var env = interpreter.env;

    const handlerSym = try SExpr.Symbol(at, "builtin-handler");
    const terminatorSym = try SExpr.Symbol(at, "terminator");
    const argsSym = try SExpr.Symbol(at, "args");

    try pushNewFrame(at, &env);
    try extendEnvFrame(at, handlerSym, try SExpr.Builtin(at, "native-handler", handler), env);
    try extendEnvFrame(at, terminatorSym, try wrapTerminator(interpreter, at, ctxId, promptSym, "native-terminator", valueTerminator), env);

    const llist = try SExpr.List(at, &[_]SExpr{ try SExpr.Symbol(at, "..."), argsSym });
    const apply = try SExpr.List(at, &[_]SExpr{try SExpr.Symbol(at, "apply"), handlerSym, argsSym });
    const closureBody = try SExpr.List(at, &[_]SExpr{ apply });

    return try SExpr.Function(at, .Lambda, llist, env, closureBody);
}


pub fn wrapTerminator(interpreter: *Interpreter, at: *const Source.Attr, ctxId: SExpr, promptName: SExpr, comptime terminatorName: []const u8, comptime terminator: fn (*Interpreter, *const Source.Attr, SExpr) Result!SExpr) Result!SExpr {
    var env = interpreter.env;

    const terminateSym = try SExpr.Symbol(at, "builtin-terminate");
    const valSym = try SExpr.Symbol(at, "val");

    try pushNewFrame(at, &env);
    try extendEnvFrame(at, terminateSym, try SExpr.Builtin(at, terminatorName, terminator), env);

    const val = try SExpr.List(at, &[_]SExpr{ try SExpr.Symbol(at, "?"), valSym });
    const llist = try SExpr.List(at, &[_]SExpr{val});
    const invoker = try SExpr.List(at, &[_]SExpr{ terminateSym, ctxId, promptName, valSym });
    const body = try SExpr.List(at, &[_]SExpr{invoker});

    return try SExpr.Function(at, .Lambda, llist, env, body);
}

pub fn valueTerminator(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Result!SExpr {
    const buf = try interpreter.expect3(args);
    const ctxId = buf[0];
    const value = try interpreter.eval(buf[2]);
    interpreter.terminationData = .{
        .ctxId = ctxId,
        .value = value,
    };
    return Signal.Terminate;
}

pub fn liftFetch(interpreter: *Interpreter, at: *const Source.Attr, name: SExpr) Result!SExpr {
    const binding =
        if (try envLookupPair(name, interpreter.evidence) orelse try frameLookup(name, interpreter.globalEvidence)) |pair| pair.forceCons().cdr else {
        return interpreter.abort(EvaluationError.MissingDynamic, at,
            "unhandled fetch `{}`", .{name});
    };
    return binding;
}

pub fn liftPrompt(interpreter: *Interpreter, at: *const Source.Attr, name: SExpr, args: SExpr) Result!SExpr {
    const handler = try liftFetch(interpreter, at, name);

    return interpreter.invoke(at, handler, args);
}

pub fn eval(interpreter: *Interpreter, sexpr: SExpr) Result!SExpr {
    switch (sexpr.getTag()) {
        inline .Nil,
        .Bool,
        .Int,
        .Char,
        .Float,
        .String,
        => {
            return sexpr;
        },

        .Symbol => {
            if (try envLookupPair(sexpr, interpreter.env)) |pair| {
                return pair.forceCons().cdr;
            } else {
                const sym = sexpr.forceSymbolSlice();

                if (std.mem.eql(u8, sym, "unquote") or std.mem.eql(u8, sym, "unquote-splicing")) {
                    return interpreter.abort(EvaluationError.InvalidContext, sexpr.getAttr(), "encountered `{s}` outside of quasi-quote", .{sym});
                } else if (std.mem.eql(u8, sym, "terminate")) {
                    return interpreter.abort(EvaluationError.UnexpectedTerminate, sexpr.getAttr(), "encountered `terminate` outside of effect handler", .{});
                } else {
                    return interpreter.abort(EvaluationError.UnboundSymbol, sexpr.getAttr(), "unbound symbol `{s}`", .{sym});
                }
            }
        },

        .Cons => {
            const xp = sexpr.forceCons();
            const fun = try interpreter.eval(xp.car);
            return @call(.always_inline, invoke, .{ interpreter, xp.attr, fun, xp.cdr });
        },

        else => return interpreter.abort(EvaluationError.NotEvaluatable, sexpr.getAttr(), "cannot evaluate {}", .{sexpr.getTag()}),
    }
}

pub fn evalListRecursive(interpreter: *Interpreter, slist: SExpr) Result!SExpr {
    const xp =
        if (slist.castCons()) |c| c
        else if (slist.isNil()) return slist
        else {
            return interpreter.abort(EvaluationError.TypeError, slist.getAttr(), "expected a list, got {}", .{slist.getTag()});
        };
    const newCar = try interpreter.eval(xp.car);
    const newCdr = try interpreter.evalListRecursive(xp.cdr);
    return try SExpr.Cons(slist.getAttr(), newCar, newCdr);
}

pub fn evalList(interpreter: *Interpreter, slist: SExpr) Result![]const SExpr {
    return evalListInRange(interpreter, slist, 0, std.math.maxInt(usize));
}

pub fn evalListInRange(interpreter: *Interpreter, slist: SExpr, minLength: usize, maxLength: usize) Result![]const SExpr {
    return evalListOfInRange(interpreter, slist, minLength, maxLength, "", passAll);
}

pub fn evalListOfInRange(interpreter: *Interpreter, slist: SExpr, minLength: usize, maxLength: usize, comptime expected: []const u8, predicate: fn (SExpr) bool) Result![]const SExpr {
    var listBuf = std.ArrayList(SExpr).init(interpreter.context.allocator);

    var tail = slist;

    while (!tail.isNil()) {
        const xp: *SExpr.Types.Cons = (tail.castCons() orelse {
            return interpreter.abort(Error.TypeError, tail.getAttr(), "expected a list, got {}", .{tail.getTag()});
        });

        tail = xp.cdr;

        const head = try interpreter.eval(xp.car);

        if (!predicate(head)) {
            return interpreter.abort(Error.TypeError, xp.car.getAttr(), "expected {s}, got {}", .{ expected, head.getTag() });
        }

        try listBuf.append(head);
    }

    const len = listBuf.items.len;

    if (len < minLength) {
        return interpreter.abort(Error.NotEnoughArguments, slist.getAttr(), "expected at least {} arguments, got {}", .{ minLength, len });
    }

    if (len > maxLength) {
        return interpreter.abort(Error.TooManyArguments, slist.getAttr(), "expected at most {} arguments, got {}", .{ maxLength, len });
    }

    listBuf.shrinkAndFree(len);

    return listBuf.items;
}

pub fn invoke(interpreter: *Interpreter, at: *const Source.Attr, fun: SExpr, sargs: SExpr) Result!SExpr {
    switch (fun.getTag()) {
        .Nil,
        .Bool,
        .Int,
        .Char,
        .Float,
        .String,
        .Symbol,
        .Cons,
        .ExternData,
        => {
            return interpreter.abort(Error.NotCallable, at,
                "expected a function or builtin, got {}: `{}`", .{fun.getTag(), fun});
        },

        .Function => {
            return @call(.always_inline, runFunction, .{ interpreter, at, fun, sargs });
        },

        .Builtin => return @call(.always_inline, runBuiltin, .{ interpreter, at, fun, sargs }),

        .ExternFunction => return @call(.always_inline, runExternFunction, .{ interpreter, at, fun, sargs }),
    }
}

fn mkLambdaListRichError(interpreter: *Interpreter, err: EvaluationError, at: *const Source.Attr, comptime fmt: []const u8, args: anytype) Error!RichError {
    return RichError{
        .err = err,
        .msg = try std.fmt.allocPrint(interpreter.context.allocator, fmt, args),
        .attr = at,
    };
}

fn mkLambdaListLiteError(_: *Interpreter, _: EvaluationError, _: *const Source.Attr, comptime _: []const u8, _: anytype) Error!void {
    return {};
}

pub const LambdaListLite = LambdaList(void, mkLambdaListLiteError);
pub const LambdaListRich = LambdaList(RichError, mkLambdaListRichError);

fn LambdaList(comptime E: type, comptime mkError: fn (*Interpreter, EvaluationError, *const Source.Attr, comptime []const u8, anytype) Result!E) type {
    return struct {
        pub const Error = E;
        pub const LLResult = union(enum) {
            Error: E,
            Okay: SExpr,
        };

        pub inline fn run(interpreter: *Interpreter, at: *const Source.Attr, list: SExpr, args: SExpr) Result!LLResult {
            var bindings = SExpr.HashMapOf(?SExpr).init(interpreter.context.allocator);
            defer bindings.deinit();

            if (try runImpl(interpreter, at, &bindings, list, args)) |err| {
                return LLResult{ .Error = err };
            } else {
                return LLResult{ .Okay = try frameFromHashMap(at, &bindings) };
            }
        }

        fn runImpl(interpreter: *Interpreter, at: *const Source.Attr, bindings: *SExpr.HashMapOf(?SExpr), expected: SExpr, given: SExpr) Result!?E {
            switch (expected.getTag()) {
                .Nil,
                .Bool,
                .Int,
                .Char,
                .Float,
                .String,
                => if (MiscUtils.equal(given, expected)) {
                    return null;
                } else {
                    return lambdaListError(interpreter, EvaluationError.TypeError, at,
                        "expected {}, got {}", .{ expected, given });
                },

                .Symbol => if (std.mem.eql(u8, expected.forceSymbolSlice(), "_")) {
                    return null;
                } else {
                    return bind(interpreter, at, bindings, expected, given);
                },

                .Cons => {
                    var xp = expected.forceCons();
                    if (xp.car.isExactSymbol("quote")) {
                        xp = xp.cdr.castCons() orelse {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "lambda list element contains invalid quote, body is {}", .{xp.cdr.getTag()});
                        };

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "lambda list element contains invalid quote, tail of body is {}", .{xp.cdr.getTag()});
                        }

                        const q = xp.car;
                        if (q.isSymbol()) {
                            if (MiscUtils.equal(q, given)) {
                                return null;
                            } else {
                                return lambdaListError(interpreter, EvaluationError.TypeError, at,
                                    "expected {}, got {}", .{ q, given });
                            }
                        } else {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "lambda list element contains invalid quote, body is {} (should be Symbol)", .{q.getTag()});
                        }
                    } else if (xp.car.isExactSymbol("unquote")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "lambda list element contains invalid unquote, body is {}", .{xp.cdr.getTag()});

                        const uq = try interpreter.eval(xp.car);

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "lambda list element contains invalid unquote, tail of body is {}", .{xp.cdr.getTag()});
                        }

                        return runImpl(interpreter, at, bindings, uq, given);
                    } else if (xp.car.isExactSymbol(":")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a value to follow `->`, got {}", .{xp.cdr.getTag()});
                        const predE = xp.car;

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected only a value to follow `:`, got {}", .{xp.cdr.getTag()});
                        }

                        const res = try interpreter.nativeInvoke(at, try interpreter.eval(predE), &[1]SExpr { given });

                        if (res.coerceNativeBool()) {
                            return null;
                        } else {
                            return lambdaListError(interpreter, EvaluationError.RangeError, at,
                                "predicate `{}` failed on value `{}`", .{ predE, given });
                        }
                    } else if (xp.car.isExactSymbol("->")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a value to follow `->`, got {}", .{xp.cdr.getTag()});
                        const predE = xp.car;

                        const args = try SExpr.Quote(try SExpr.List(at, &[_]SExpr { given }));
                        const body = try SExpr.List(at, &[_]SExpr { try SExpr.Symbol(at, "apply"), predE, args });

                        var out: NativeWithOut = undefined;
                        try interpreter.nativeWith(at, "fail", struct {
                            fn fun(e: *Interpreter, a: *const Source.Attr, x: SExpr) Result!SExpr {
                                const terminator = try envLookup(try SExpr.Symbol(a, "terminator"), e.env) orelse {
                                    return e.abort(EvaluationError.InvalidContext, a,
                                        "missing terminator in lambda list fail handler", .{});
                                };
                                return try invoke(e, a, terminator, x);
                            }
                        }.fun, body, &out);

                        switch (out) {
                            .Evaluated => |x| {
                                if (!xp.cdr.isNil()) {
                                    return runImpl(interpreter, at, bindings, xp.cdr, x);
                                } else {
                                    return null;
                                }
                            },
                            .Terminated => |_| {
                                return lambdaListError(interpreter, EvaluationError.RangeError, at,
                                    "view pattern `{}` failed on value `{}`", .{ predE, given });
                            },
                        }
                    } else if (xp.car.isExactSymbol("?")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, at,
                            "invalid lambda list, expected a var to follow `?`, got {}", .{xp.cdr.getTag()});
                        const vx = xp.car;

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, at,
                                "invalid lambda list, expected only a value to follow `?`, got {}", .{xp.cdr.getTag()});
                        }

                        if (given.isNil()) {
                            const varBinders = try binders(interpreter, at, vx);
                            defer interpreter.context.allocator.free(varBinders);

                            for (varBinders) |binder| {
                                // cannot fail because given is null
                                _ = try bind(interpreter, at, bindings, binder, null);
                            }

                            return null;
                        } else {
                            return runImpl(interpreter, at, bindings, vx, given);
                        }
                    } else if (xp.car.isExactSymbol("@")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a var to follow `@`, got {}", .{xp.cdr.getTag()});
                        const atSym = xp.car;

                        if (!atSym.isSymbol()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected a symbol to follow `@`, got {}", .{atSym.getTag()});
                        }

                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a value to follow symbol in `@`, got {}", .{xp.cdr.getTag()});
                        const vx = xp.car;

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected only a value to follow `@`, got {}", .{xp.cdr.getTag()});
                        }

                        if (try runImpl(interpreter, at, bindings, vx, given)) |err| {
                            return err;
                        }

                        return bind(interpreter, at, bindings, atSym, given);
                    } else if (xp.car.isExactSymbol("...")) {
                        xp = xp.cdr.castCons() orelse {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected a symbol to follow `...`, got {}", .{xp.cdr.getTag()});
                        };
                        var restSym = xp.car;

                        const invalid = invalid: {
                            if (!restSym.isSymbol()) {
                                if (restSym.castCons()) |rxp| {
                                    if (rxp.car.isExactSymbol("unquote")) {
                                        const xp2 = rxp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                            "invalid lambda list, malformed unquote in rest parameter, got {}", .{rxp.cdr.getTag()});
                                        restSym = try interpreter.eval(xp2.car);

                                        if (!xp2.cdr.isNil()) {
                                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                                "invalid lambda list, expected only a value to follow `,` in rest parameter unquote, got {}", .{xp2.cdr.getTag()});
                                        }

                                        if (!restSym.isSymbol()) {
                                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                                "invalid lambda list, expected unquote inside rest parameter to evaluate to a symbol, got {}", .{restSym.getTag()});
                                        }

                                        break :invalid false;
                                    }
                                }

                                break :invalid true;
                            }

                            break :invalid false;
                        };

                        if (invalid) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected a symbol to follow `...`, got {}", .{restSym.getTag()});
                        }

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected only a symbol to follow `...`, got {}", .{xp.cdr.getTag()});
                        }

                        return try bind(interpreter, at, bindings, restSym, given);
                    }

                    var currentExpected = expected;
                    var currentGiven = given;
                    while (!currentExpected.isNil()) {
                        const xpExpected = currentExpected.castCons() orelse { // MiscUtils pairs of the form `(a . a)`
                            return runImpl(interpreter, at, bindings, currentExpected, currentGiven);
                        };

                        const elemExpected = xpExpected.car;

                        if (isRest(elemExpected)) {
                            if (try runImpl(interpreter, at, bindings, elemExpected, currentGiven)) |err| {
                                return err;
                            }

                            if (!xpExpected.cdr.isNil()) {
                                return interpreter.abort(EvaluationError.TypeError, xpExpected.attr,
                                    "invalid lambda list, expected rest parameter to end list got {}", .{xpExpected.cdr.getTag()});
                            }

                            return null;
                        }

                        if (currentGiven.castCons()) |xpGiven| {
                            const elemGiven = xpGiven.car;

                            if (try runImpl(interpreter, at, bindings, elemExpected, elemGiven)) |err| {
                                return err;
                            }

                            currentGiven = xpGiven.cdr;
                        } else if (currentGiven.isNil() and isOptional(elemExpected)) {
                            if (try runImpl(interpreter, at, bindings, elemExpected, currentGiven)) |err| {
                                return err;
                            }
                        } else {
                            return lambdaListError(interpreter, EvaluationError.NotEnoughArguments, at,
                                "expected more items in input list; comparing `{}` to `{}`", .{expected, given});
                        }

                        currentExpected = xpExpected.cdr;
                    }

                    if (currentGiven.isNil()) {
                        return null;
                    } else if (currentGiven.isCons()) {
                        return lambdaListError(interpreter, EvaluationError.TooManyArguments, at,
                            "expected less items in input list; comparing `{}` to `{}` (leaves `{}`)", .{expected, given, currentGiven});
                    } else {
                        return lambdaListError(interpreter, EvaluationError.TypeError, at,
                            "expected a list, got {}; comparing `{}` to `{}`", .{currentGiven.getTag(), expected, given});
                    }
                },

                .Function,
                .Builtin,
                .ExternData,
                .ExternFunction,
                => {
                    return interpreter.abort(EvaluationError.TypeError, at,
                        "invalid lambda list element ({} is not supported)", .{expected.getTag()});
                },
            }
        }

        fn isRest(list: SExpr) bool {
            if (list.castCons()) |xp| {
                return xp.car.isExactSymbol("...");
            }

            return false;
        }

        fn isOptional(list: SExpr) bool {
            if (list.castCons()) |xp| {
                if (xp.car.isExactSymbol("?") or xp.car.isExactSymbol("...")) {
                    return true;
                } else if (xp.car.isExactSymbol("@")) {
                    if (xp.cdr.castCons()) |xp2| {
                        if (xp2.cdr.castCons()) |xp3| {
                            return isOptional(xp3.car);
                        }
                    }
                }
            }

            return false;
        }

        inline fn bind(interpreter: *Interpreter, at: *const Source.Attr, bindings: *SExpr.HashMapOf(?SExpr), symbol: SExpr, given: ?SExpr) Result!?E {
            std.debug.assert(symbol.isSymbol());

            if (bindings.get(symbol)) |existing| {
                if (existing) |e| {
                    if (given) |g| {
                        if (!MiscUtils.equal(e, g)) {
                            return lambdaListError(interpreter, EvaluationError.TypeError, at, "expected {}, got {}", .{ e, g });
                        }
                    }
                } else {
                    try bindings.put(symbol, given);
                }
            } else {
                try bindings.put(symbol, given);
            }

            return null;
        }

        pub inline fn binders(interpreter: *Interpreter, at: *const Source.Attr, expected: SExpr) Result![]SExpr {
            var set = SExpr.HashSet.init(interpreter.context.allocator);
            defer set.deinit();

            try bindersImpl(interpreter, at, expected, &set);

            return try interpreter.context.allocator.dupe(SExpr, set.keys());
        }

        pub fn validate(interpreter: *Interpreter, expect: SExpr) Result!void {
            var set = SExpr.HashSet.init(interpreter.context.allocator);
            defer set.deinit();

            try bindersImpl(interpreter, expect.getAttr(), expect, &set);
        }

        fn bindersImpl(interpreter: *Interpreter, at: *const Source.Attr, expected: SExpr, out: *SExpr.HashSet) Result!void {
            switch (expected.getTag()) {
                inline .Nil,
                .Bool,
                .Int,
                .Char,
                .Float,
                .String,
                => return,

                .Symbol => {
                    if (!out.contains(expected)) {
                        try out.put(expected, {});
                    }
                    return;
                },

                .Cons => {
                    var xp = expected.forceCons();
                    if (xp.car.isExactSymbol("quote")) {
                        xp = xp.cdr.castCons() orelse {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "lambda list element contains invalid quote, body is {}", .{xp.cdr.getTag()});
                        };

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "lambda list element contains invalid quote, tail of body is {}", .{xp.cdr.getTag()});
                        }

                        const q = xp.car;
                        if (q.isSymbol()) {
                            return;
                        } else {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "lambda list element contains invalid quote, body is {} (should be symbol)", .{q.getTag()});
                        }
                    } else if (xp.car.isExactSymbol("unquote")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "lambda list element contains invalid unquote, body is {}", .{xp.cdr.getTag()});

                        const uq = try interpreter.eval(xp.car);

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "lambda list element contains invalid unquote, tail of body is {}", .{xp.cdr.getTag()});
                        }

                        return bindersImpl(interpreter, at, uq, out);
                    } else if (xp.car.isExactSymbol(":")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a value to follow `:`, got {}", .{xp.cdr.getTag()});

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected only a value to follow `:`, got {}", .{xp.cdr.getTag()});
                        }

                        return;
                    } else if (xp.car.isExactSymbol("->")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a value to follow `->`, got {}", .{xp.cdr.getTag()});

                        return bindersImpl(interpreter, at, xp.cdr, out);
                    } else if (xp.car.isExactSymbol("?")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a var to follow `?`, got {}", .{xp.cdr.getTag()});
                        const vx = xp.car;

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected only a value to follow `?`, got {}", .{xp.cdr.getTag()});
                        }

                        return bindersImpl(interpreter, at, vx, out);
                    } else if (xp.car.isExactSymbol("@")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a var to follow `@`, got {}", .{xp.cdr.getTag()});
                        const atSym = xp.car;

                        if (!atSym.isSymbol()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected a symbol to follow `@`, got {}", .{atSym.getTag()});
                        }

                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, at,
                            "invalid lambda list, expected a value to follow symbol in `@`, got {}", .{xp.cdr.getTag()});
                        const vx = xp.car;

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected only a value to follow `@`, got {}", .{xp.cdr.getTag()});
                        }

                        if (!out.contains(atSym)) {
                            try out.put(atSym, {});
                        }

                        return bindersImpl(interpreter, at, vx, out);
                    } else if (xp.car.isExactSymbol("...")) {
                        xp = xp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                            "invalid lambda list, expected a symbol to follow `...`, got {}", .{xp.cdr.getTag()});
                        var restSym = xp.car;

                        const invalid = invalid: {
                            if (!restSym.isSymbol()) {
                                if (restSym.castCons()) |rxp| {
                                    if (rxp.car.isExactSymbol("unquote")) {
                                        const xp2 = rxp.cdr.castCons() orelse return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                            "invalid lambda list, malformed unquote in rest parameter, got {}", .{rxp.cdr.getTag()});
                                        restSym = try interpreter.eval(xp2.car);

                                        if (!xp2.cdr.isNil()) {
                                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                                "invalid lambda list, expected only a value to follow `,` in rest parameter unquote, got {}", .{xp2.cdr.getTag()});
                                        }

                                        if (!restSym.isSymbol()) {
                                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                                "invalid lambda list, expected unquote inside rest parameter to evaluate to a symbol, got {}", .{restSym.getTag()});
                                        }

                                        break :invalid false;
                                    }
                                }

                                break :invalid true;
                            }

                            break :invalid false;
                        };

                        if (invalid) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected a symbol to follow `...`, got {}", .{restSym.getTag()});
                        }

                        if (!xp.cdr.isNil()) {
                            return interpreter.abort(EvaluationError.TypeError, xp.attr,
                                "invalid lambda list, expected only a symbol to follow `...`, got {}", .{xp.cdr.getTag()});
                        }

                        if (!out.contains(restSym)) {
                            try out.put(restSym, {});
                        }

                        return;
                    }

                    var currentExpected = expected;
                    while (!currentExpected.isNil()) {
                        const xpExpected = currentExpected.castCons() orelse { // MiscUtils pairs of the form `(a . a)`
                            return bindersImpl(interpreter, at, currentExpected, out);
                        };

                        const elemExpected = xpExpected.car;

                        try bindersImpl(interpreter, at, elemExpected, out);

                        currentExpected = xpExpected.cdr;
                    }
                },

                inline .Function,
                .Builtin,
                .ExternData,
                .ExternFunction,
                => {
                    return interpreter.abort(EvaluationError.TypeError, at,
                        "invalid lambda list element ({} is not supported)", .{expected.getTag()});
                },
            }
        }

        inline fn lambdaListAbort(interpreter: *Interpreter, err: EvaluationError, at: *const Source.Attr, comptime fmt: []const u8, args: anytype) Result!LLResult {
            return LLResult{
                .Error = (try lambdaListError(interpreter, err, at, fmt, args)).?,
            };
        }

        inline fn lambdaListError(interpreter: *Interpreter, err: EvaluationError, at: *const Source.Attr, comptime fmt: []const u8, args: anytype) Result!?E {
            return try @call(.always_inline, mkError, .{ interpreter, err, at, fmt, args });
        }
    };
}

pub fn frameFromHashMap(at: *const Source.Attr, map: *const SExpr.HashMapOf(?SExpr)) Error!SExpr {
    const nil = try SExpr.Nil(at);
    var frame = nil;

    var iter = map.iterator();
    while (iter.next()) |rawPair| {
        const key = rawPair.key_ptr.*;
        const value = if (rawPair.value_ptr.*) |val| val else nil;
        const pair = try SExpr.Cons(at, key, value);
        frame = try SExpr.Cons(at, pair, frame);
    }

    return frame;
}

pub fn envFromHashMap(at: *const Source.Attr, map: *const SExpr.HashMapOf(?SExpr)) Error!SExpr {
    const frame = try frameFromHashMap(at, map);

    return try SExpr.Cons(at, frame, try SExpr.Nil(at));
}

pub fn runFunction(interpreter: *Interpreter, at: *const Source.Attr, sfun: SExpr, args: SExpr) Result!SExpr {
    Core.log.debug("call depth {}\n", .{interpreter.callDepth});
    if (interpreter.callDepth > Config.MAX_DEPTH) {
        return interpreter.exit(Error.CallStackOverflow, at);
    }

    interpreter.callDepth += 1;
    defer interpreter.callDepth -= 1;

    const fun = sfun.forceFunction();

    const eArgs = switch (fun.kind) {
        .Lambda => try interpreter.evalListRecursive(args),
        .Macro => args,
    };

    const frame = switch (try LambdaListRich.run(interpreter, at, fun.args, eArgs)) {
        .Okay => |frame| frame,
        .Error => |rich| return interpreter.abort(rich.err, rich.attr orelse at,
            "{s}", .{rich.msg orelse "failed to bind lambda list"}),
    };

    const result = result: {
        const callerEnv = interpreter.env;
        interpreter.env = fun.env;

        const oldCallerEnv = interpreter.callerEnv;
        interpreter.callerEnv = callerEnv;

        defer {
            interpreter.env = callerEnv;
            interpreter.callerEnv = oldCallerEnv;
        }

        try pushFrame(frame, &interpreter.env);

        break :result try @call(.always_inline, runProgram, .{ interpreter, fun.body });
    };

    return switch (fun.kind) {
        .Lambda => result,
        .Macro => try interpreter.eval(result),
    };
}

pub fn runBuiltin(interpreter: *Interpreter, at: *const Source.Attr, sbuiltin: SExpr, sargs: SExpr) Result!SExpr {
    const builtin = sbuiltin.forceBuiltin();
    return try builtin.getProc()(interpreter, at, sargs);
}

pub fn runExternFunction(interpreter: *Interpreter, at: *const Source.Attr, sexternFunction: SExpr, sargs: SExpr) Result!SExpr {
    const externFunction = sexternFunction.forceExternFunction();

    var msg: ExternMessage = undefined;
    var out: SExpr = undefined;

    if (externFunction.proc(interpreter, at, &msg, &out, sargs)) {
        return out;
    } else {
        return resultFromExtern(msg);
    }
}

pub fn runProgram(interpreter: *Interpreter, program: SExpr) Result!SExpr {
    var tail = program;
    var result = try SExpr.Nil(interpreter.context.attr);

    while (!tail.isNil()) {
        const list: *SExpr.Types.Cons = (tail.castCons() orelse {
            return interpreter.abort(EvaluationError.TypeError, tail.getAttr(), "expected an expression list, got {}", .{tail.getTag()});
        });

        result = try interpreter.eval(list.car);

        tail = list.cdr;
    }

    return result;
}

pub fn resultFromExtern(err: ExternMessage) Result {
    switch (err) {
        ExternMessage.SigTerminate => return Signal.Terminate,

        ExternMessage.ErrPanic => return EvaluationError.Panic,
        ExternMessage.ErrNotEvaluatable => return EvaluationError.NotEvaluatable,
        ExternMessage.ErrNotCallable => return EvaluationError.NotCallable,
        ExternMessage.ErrTypeError => return EvaluationError.TypeError,
        ExternMessage.ErrRangeError => return EvaluationError.RangeError,
        ExternMessage.ErrNotEnoughArguments => return EvaluationError.NotEnoughArguments,
        ExternMessage.ErrTooManyArguments => return EvaluationError.TooManyArguments,
        ExternMessage.ErrDivisionByZero => return EvaluationError.DivisionByZero,
        ExternMessage.ErrUnboundSymbol => return EvaluationError.UnboundSymbol,
        ExternMessage.ErrInvalidContext => return EvaluationError.InvalidContext,
        ExternMessage.ErrEnvironmentUnderflow => return EvaluationError.EnvironmentUnderflow,
        ExternMessage.ErrCallStackOverflow => return EvaluationError.CallStackOverflow,
        ExternMessage.ErrMissingDynamic => return EvaluationError.MissingDynamic,
        ExternMessage.ErrUnexpectedTerminate => return EvaluationError.UnexpectedTerminate,
        ExternMessage.ErrMissingTerminationData => return EvaluationError.MissingTerminationData,

        ExternMessage.ErrOutOfMemory => return Context.Error.OutOfMemory,
        ExternMessage.ErrBadEncoding => return TextUtils.Error.BadEncoding,
    }
}

pub fn externFromResult(res: Result) ExternMessage {
    switch (res) {
        Signal.Terminate => return ExternMessage.SigTerminate,

        EvaluationError.Panic => return ExternMessage.ErrPanic,
        EvaluationError.NotEvaluatable => return ExternMessage.ErrNotEvaluatable,
        EvaluationError.NotCallable => return ExternMessage.ErrNotCallable,
        EvaluationError.TypeError => return ExternMessage.ErrTypeError,
        EvaluationError.RangeError => return ExternMessage.ErrRangeError,
        EvaluationError.NotEnoughArguments => return ExternMessage.ErrNotEnoughArguments,
        EvaluationError.TooManyArguments => return ExternMessage.ErrNotEnoughArguments,
        EvaluationError.DivisionByZero => return ExternMessage.ErrDivisionByZero,
        EvaluationError.UnboundSymbol => return ExternMessage.ErrUnboundSymbol,
        EvaluationError.InvalidContext => return ExternMessage.ErrInvalidContext,
        EvaluationError.EnvironmentUnderflow => return ExternMessage.ErrEnvironmentUnderflow,
        EvaluationError.CallStackOverflow => return ExternMessage.ErrCallStackOverflow,
        EvaluationError.MissingDynamic => return ExternMessage.ErrMissingDynamic,
        EvaluationError.UnexpectedTerminate => return ExternMessage.ErrUnexpectedTerminate,
        EvaluationError.MissingTerminationData => return ExternMessage.ErrMissingTerminationData,

        Context.Error.OutOfMemory => return ExternMessage.ErrOutOfMemory,
    }
}

pub fn validateNil(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isNil()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected Nil, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateInt(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isInt()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected an Int, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateBool(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isBool()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Bool, got {}: `{}", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateChar(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isChar()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Char, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateFloat(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isFloat()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Float, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateString(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isString()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a String, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateStringSlice(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr, slice: []const u8) Result!void {
    if (!sexpr.isExactString(slice)) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a String `{}`, got {}: `{}`", .{ slice, sexpr.getTag(), sexpr });
    }
}

pub fn validateSymbol(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isSymbol()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Symbol, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateSymbolSlice(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr, slice: []const u8) Result!void {
    if (!sexpr.isExactSymbol(slice)) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Symbol `{s}`, got {}: `{}`", .{ slice, sexpr.getTag(), sexpr });
    }
}

pub fn validatePair(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isCons()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a pair, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateList(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isCons()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a list, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}
pub fn validateListOrNil(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isCons() and !sexpr.isNil()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a list, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateFunction(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isFunction()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a function, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateBuiltin(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isBuiltin()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Builtin, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateExternData(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isExternData()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected an ExternData, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateExternDataPtr(interpreter: *Interpreter, comptime T: type, at: *const Source.Attr, sexpr: SExpr) Result!void {
    const externData = try interpreter.castExternData(at, sexpr);

    if (externData.castPtr(T) == null) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected an ExternData of type {s}, got {s}", .{ @typeName(T), externData.typeNameSlice() });
    }
}

pub fn validateExternFunction(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isExternFunction()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected an ExternFunction, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn validateCallable(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!void {
    if (!sexpr.isCallable()) {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a callable, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castNil(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!MiscUtils.Unit {
    if (sexpr.castNil()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected Nil, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castInt(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!i64 {
    if (sexpr.castInt()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected an Int, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castBool(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!bool {
    if (sexpr.castBool()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Bool, got {}: `{}", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castChar(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!TextUtils.Char {
    if (sexpr.castChar()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Char, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castFloat(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!f64 {
    if (sexpr.castFloat()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Float, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castString(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!*SExpr.Types.String {
    if (sexpr.castString()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a String, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castStringSlice(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result![]const u8 {
    const str = try interpreter.castString(at, sexpr);

    return str.toSlice();
}

pub fn castSymbol(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!*SExpr.Types.Symbol {
    if (sexpr.castSymbol()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Symbol, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castSymbolSlice(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result![]const u8 {
    const sym = try interpreter.castSymbol(at, sexpr);

    return sym.toSlice();
}

pub fn castPair(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!*SExpr.Types.Cons {
    if (sexpr.castCons()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a pair, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castPairTuple(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!struct { SExpr, SExpr } {
    const pair = try interpreter.castPair(at, sexpr);

    return .{ pair.car, pair.cdr };
}

pub fn castList(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!*SExpr.Types.Cons {
    if (sexpr.castCons()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a list, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castFunction(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!*SExpr.Types.Function {
    if (sexpr.castFunction()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a function, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castBuiltin(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!*SExpr.Types.Builtin {
    if (sexpr.castBuiltin()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Builtin, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castExternData(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!*SExpr.Types.ExternData {
    if (sexpr.castExternData()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected an ExternData, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn castExternDataPtr(interpreter: *Interpreter, comptime T: type, at: *const Source.Attr, sexpr: SExpr) Result!*T {
    const externData = try interpreter.castExternData(at, sexpr);

    return externData.castPtr(T) orelse {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected an ExternData of type {s}, got {s}", .{ @typeName(T), externData.typeNameSlice() });
    };
}

pub fn castExternFunction(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!*SExpr.Types.ExternFunction {
    if (sexpr.castExternFunction()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected an ExternFunction, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn coerceNativeInt(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!i64 {
    if (sexpr.coerceNativeInt()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a number, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn coerceNativeFloat(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!f64 {
    if (sexpr.coerceNativeFloat()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a number, got {}: `{}", .{ sexpr.getTag(), sexpr });
    }
}

pub fn coerceNativeChar(interpreter: *Interpreter, at: *const Source.Attr, sexpr: SExpr) Result!TextUtils.Char {
    if (sexpr.coerceNativeChar()) |x| {
        return x;
    } else {
        return interpreter.abort(Interpreter.Error.TypeError, at,
            "expected a Char, got {}: `{}`", .{ sexpr.getTag(), sexpr });
    }
}

pub fn errorToException(interpreter: *Interpreter, at: *const Source.Attr, err: anyerror) Result!SExpr {
    return interpreter.nativePrompt(at,
        "exception", &[_]SExpr{try SExpr.Symbol(at, @errorName(err))});
}

pub inline fn argIterator(interpreter: *Interpreter, shouldInterpreter: bool, args: SExpr) Result!ArgIterator {
    return ArgIterator.init(interpreter, shouldInterpreter, args);
}

pub const ArgIterator = struct {
    interpreter: *Interpreter,
    at: *const Source.Attr,
    shouldInterpreter: bool,
    tail: SExpr,
    index: usize,

    pub fn init(interpreter: *Interpreter, shouldInterpreter: bool, args: SExpr) Interpreter.Result!ArgIterator {
        const at = args.getAttr();
        if (args.isNil() or args.isCons()) {
            return .{
                .interpreter = interpreter,
                .at = at,
                .shouldInterpreter = shouldInterpreter,
                .tail = args,
                .index = 0,
            };
        } else {
            return interpreter.abort(Interpreter.Error.TypeError, at,
                "expected an argument list, got {}: `{}`", .{ args.getTag(), args });
        }
    }

    pub fn next(self: *ArgIterator) Interpreter.Result!?SExpr {
        if (self.tail.isNil()) {
            return null;
        }

        const xp = try self.interpreter.castList(self.at, self.tail);

        self.index += 1;

        self.tail = xp.cdr;

        return if (self.shouldInterpreter) try self.interpreter.eval(xp.car) else xp.car;
    }

    pub fn atLeast(self: *ArgIterator) Interpreter.Result!SExpr {
        const arg = try self.next();

        if (arg) |a| {
            return a;
        } else {
            return self.interpreter.abort(Interpreter.Error.NotEnoughArguments, self.at,
                "expected at least {} argument(s)", .{self.index + 1});
        }
    }

    pub fn hasNext(self: *ArgIterator) bool {
        return !self.tail.isNil();
    }

    pub fn assertDone(self: *ArgIterator) Result!void {
        if (!self.tail.isNil()) {
            if (self.tail.isCons()) {
                return self.interpreter.abort(Interpreter.Error.TooManyArguments, self.at,
                    "expected at most {} arguments, got {}", .{ self.index, self.index + 1 });
            } else {
                return self.interpreter.abort(Interpreter.Error.TypeError, self.at,
                    "expected an argument list, got {}: `{}`", .{ self.tail.getTag(), self.tail });
            }
        }
    }

    pub fn nextWithIndex(self: *ArgIterator) Interpreter.Result!?struct { SExpr, usize } {
        const i = self.index;
        return .{ try self.next() orelse return null, i };
    }
};

pub fn expect0(interpreter: *Interpreter, args: SExpr) Result!void {
    if (!args.isNil()) {
        if (args.isCons()) {
            return interpreter.abort(Interpreter.Error.TooManyArguments, args.getAttr(), "expected no arguments, got: `{}`", .{args});
        } else {
            return interpreter.abort(Interpreter.Error.TypeError, args.getAttr(), "expected an empty argument list, got {}: `{}`", .{ args.getTag(), args });
        }
    }
}

pub fn expect1(interpreter: *Interpreter, args: SExpr) Result!SExpr {
    var eArgs = [1]SExpr{undefined};
    _ = try interpreter.expectSmallList(args, 1, &eArgs);
    return eArgs[0];
}

pub fn expect2(interpreter: *Interpreter, args: SExpr) Result![2]SExpr {
    var eArgs = [2]SExpr{ undefined, undefined };
    _ = try interpreter.expectSmallList(args, 2, &eArgs);
    return eArgs;
}

pub fn expect3(interpreter: *Interpreter, args: SExpr) Result![3]SExpr {
    var eArgs = [3]SExpr{ undefined, undefined, undefined };
    _ = try interpreter.expectSmallList(args, 3, &eArgs);
    return eArgs;
}

pub fn expect4(interpreter: *Interpreter, args: SExpr) Result![4]SExpr {
    var eArgs = [4]SExpr{ undefined, undefined, undefined, undefined };
    _ = try interpreter.expectSmallList(args, 4, &eArgs);
    return eArgs;
}

pub fn expectAtLeast1(interpreter: *Interpreter, args: SExpr) Result!struct { head: SExpr, tail: SExpr } {
    var eArgs = [1]SExpr{undefined};
    const tail = try interpreter.expectSmallListAtLeast(args, &eArgs);
    return .{ .head = eArgs[0], .tail = tail };
}

pub fn expectAtLeast2(interpreter: *Interpreter, args: SExpr) Result!struct { head: [2]SExpr, tail: SExpr } {
    var eArgs = [2]SExpr{ undefined, undefined };
    const tail = try interpreter.expectSmallListAtLeast(args, &eArgs);
    return .{ .head = eArgs, .tail = tail };
}

pub fn expectAtLeast3(interpreter: *Interpreter, args: SExpr) Result!struct { head: [3]SExpr, tail: SExpr } {
    var eArgs = [3]SExpr{ undefined, undefined, undefined };
    const tail = try interpreter.expectSmallListAtLeast(args, &eArgs);
    return .{ .head = eArgs, .tail = tail };
}

pub fn expectAtLeast4(interpreter: *Interpreter, args: SExpr) Result!struct { head: [4]SExpr, tail: SExpr } {
    var eArgs = [4]SExpr{ undefined, undefined, undefined, undefined };
    const tail = try interpreter.expectSmallListAtLeast(args, &eArgs);
    return .{ .head = eArgs, .tail = tail };
}

pub fn expectMaybe1(interpreter: *Interpreter, args: SExpr) Result!?SExpr {
    var eArgs = [1]SExpr{undefined};
    const len = try interpreter.expectSmallList(args, 0, &eArgs);
    return if (len == 1) return eArgs[0] else null;
}

pub fn eval1(interpreter: *Interpreter, args: SExpr) Result!SExpr {
    var eArgs = [1]SExpr{undefined};
    _ = try interpreter.evalSmallList(args, 1, &eArgs);
    return eArgs[0];
}

pub fn eval2(interpreter: *Interpreter, args: SExpr) Result![2]SExpr {
    var eArgs = [2]SExpr{ undefined, undefined };
    _ = try interpreter.evalSmallList(args, 2, &eArgs);
    return eArgs;
}

pub fn eval3(interpreter: *Interpreter, args: SExpr) Result![3]SExpr {
    var eArgs = [3]SExpr{ undefined, undefined, undefined };
    _ = try interpreter.evalSmallList(args, 3, &eArgs);
    return eArgs;
}

pub fn eval4(interpreter: *Interpreter, args: SExpr) Result![4]SExpr {
    var eArgs = [4]SExpr{ undefined, undefined, undefined, undefined };
    _ = try interpreter.evalSmallList(args, 4, &eArgs);
    return eArgs;
}

pub fn evalMaybe1(interpreter: *Interpreter, args: SExpr) Result!?SExpr {
    var eArgs = [1]SExpr{undefined};
    const len = try interpreter.expectSmallList(args, 0, &eArgs);
    return if (len == 1) return try interpreter.eval(eArgs[0]) else null;
}

pub fn evalAtLeast1(interpreter: *Interpreter, args: SExpr) Result!struct { head: SExpr, tail: SExpr } {
    var eArgs = [1]SExpr{undefined};
    const tail = try interpreter.evalSmallListAtLeast(args, &eArgs);
    return .{ .head = eArgs[0], .tail = tail };
}

pub fn evalAtLeast2(interpreter: *Interpreter, args: SExpr) Result!struct { head: [2]SExpr, tail: SExpr } {
    var eArgs = [2]SExpr{ undefined, undefined };
    const tail = try interpreter.evalSmallListAtLeast(args, &eArgs);
    return .{ .head = eArgs, .tail = tail };
}

pub fn evalAtLeast3(interpreter: *Interpreter, args: SExpr) Result!struct { head: [3]SExpr, tail: SExpr } {
    var eArgs = [3]SExpr{ undefined, undefined, undefined };
    const tail = try interpreter.evalSmallListAtLeast(args, &eArgs);
    return .{ .head = eArgs, .tail = tail };
}

pub fn evalAtLeast4(interpreter: *Interpreter, args: SExpr) Result!struct { head: [4]SExpr, tail: SExpr } {
    var eArgs = [4]SExpr{ undefined, undefined, undefined, undefined };
    const tail = try interpreter.evalSmallListAtLeast(args, &eArgs);
    return .{ .head = eArgs, .tail = tail };
}

pub fn expectSmallListOf(interpreter: *Interpreter, sexpr: SExpr, minLength: usize, buf: []SExpr, comptime expected: []const u8, comptime predicate: fn (SExpr) bool) Result!usize {
    var tail = sexpr;
    var i: usize = 0;

    while (!tail.isNil()) {
        const cons = (tail.castCons() orelse {
            return interpreter.abort(Error.TypeError, tail.getAttr(), "expected a list, got {}", .{tail.getTag()});
        });

        tail = cons.cdr;

        const elem = cons.car;

        if (!@call(.always_inline, predicate, .{elem})) {
            return interpreter.abort(Error.TypeError, tail.getAttr(), "expected {s}, got {}", .{ expected, elem.getTag() });
        }

        if (i < buf.len) {
            buf[i] = elem;
        }

        i += 1;
    }

    if (minLength == 0 and i == 0) {
        if (!tail.isNil()) {
            return interpreter.abort(Error.TypeError, tail.getAttr(), "expected a list, got {}", .{tail.getTag()});
        }
    }

    if (i > buf.len) {
        return interpreter.abort(Error.TooManyArguments, sexpr.getAttr(), "expected at most {} arguments, got {}", .{ buf.len, i });
    }

    if (i < minLength) {
        return interpreter.abort(Error.NotEnoughArguments, sexpr.getAttr(), "expected at least {} arguments, got {}", .{ minLength, i });
    }

    return i;
}

pub fn expectSmallListOfAtLeast(interpreter: *Interpreter, sexpr: SExpr, buf: []SExpr, comptime expected: []const u8, comptime predicate: fn (SExpr) bool) Result!SExpr {
    var tail = sexpr;
    var i: usize = 0;

    while (!tail.isNil() and i < buf.len) {
        const cons = (tail.castCons() orelse {
            return interpreter.abort(Error.TypeError, tail.getAttr(), "expected a list, got {}", .{tail.getTag()});
        });

        tail = cons.cdr;

        const elem = cons.car;

        if (!@call(.always_inline, predicate, .{elem})) {
            return interpreter.abort(Error.TypeError, elem.getAttr(), "expected {s}, got {}", .{ expected, elem.getTag() });
        }

        buf[i] = elem;
        i += 1;
    }

    if (i < buf.len) {
        return interpreter.abort(Error.NotEnoughArguments, sexpr.getAttr(), "expected at least {} arguments, got {}", .{ buf.len, i });
    }

    return tail;
}

pub fn evalSmallListAtLeast(interpreter: *Interpreter, sexpr: SExpr, buf: []SExpr) Result!SExpr {
    const tail = try interpreter.expectSmallListAtLeast(sexpr, buf);
    for (0..buf.len) |i| {
        buf[i] = try interpreter.eval(buf[i]);
    }
    return tail;
}

pub fn expectSmallListAtLeast(interpreter: *Interpreter, sexpr: SExpr, buf: []SExpr) Result!SExpr {
    return expectSmallListOfAtLeast(interpreter, sexpr, buf, "", passAll);
}

pub fn expectLongListOfAtLeast(interpreter: *Interpreter, sexpr: SExpr, minLength: usize, comptime expected: []const u8, comptime predicate: fn (SExpr) bool) Result!SExpr {
    var tail = sexpr;
    var i: usize = 0;

    while (!tail.isNil() and i < minLength) {
        const cons = (tail.castCons() orelse {
            return interpreter.abort(Error.TypeError, tail.getAttr(), "expected a list, got {}", .{tail.getTag()});
        });

        tail = cons.cdr;

        const elem = cons.car;

        if (!@call(.always_inline, predicate, .{elem})) {
            return interpreter.abort(Error.TypeError, elem.getAttr(), "expected {s}, got {}", .{ expected, elem.getTag() });
        }

        i += 1;
    }

    if (i < minLength) {
        return interpreter.abort(Error.NotEnoughArguments, sexpr.getAttr(), "expected at least {} arguments, got {}", .{ minLength, i });
    }

    return tail;
}

pub fn expectLongListAtLeast(interpreter: *Interpreter, sexpr: SExpr, minLength: usize) Result!SExpr {
    return expectLongListOfAtLeast(interpreter, sexpr, minLength, "", passAll);
}

pub fn expectLongListOf(interpreter: *Interpreter, sexpr: SExpr, minLength: usize, maxLength: usize, comptime expected: []const u8, comptime predicate: fn (SExpr) bool) Result!usize {
    var tail = sexpr;
    var i: usize = 0;

    while (!tail.isNil()) {
        const cons = (tail.castCons() orelse {
            return interpreter.abort(Error.TypeError, tail.getAttr(), "expected a list, got {}", .{tail.getTag()});
        });

        tail = cons.cdr;

        const elem = cons.car;

        if (!@call(.always_inline, predicate, .{elem})) {
            return interpreter.abort(Error.TypeError, elem.getAttr(), "expected {s}, got {}", .{ expected, elem.getTag() });
        }

        i += 1;
    }

    if (i > maxLength) {
        return interpreter.abort(Error.TooManyArguments, sexpr.getAttr(), "expected at most {} arguments, got {}", .{ maxLength, i });
    }

    if (i < minLength) {
        return interpreter.abort(Error.NotEnoughArguments, sexpr.getAttr(), "expected at least {} arguments, got {}", .{ minLength, i });
    }

    return i;
}

fn passAll(_: SExpr) bool {
    return true;
}

pub fn evalSmallList(interpreter: *Interpreter, sexpr: SExpr, minLength: usize, buf: []SExpr) Result!usize {
    const len = try expectSmallList(interpreter, sexpr, minLength, buf);
    for (0..len) |i| {
        buf[i] = try interpreter.eval(buf[i]);
    }
    return len;
}

pub fn expectSmallList(interpreter: *Interpreter, sexpr: SExpr, minLength: usize, buf: []SExpr) Result!usize {
    return expectSmallListOf(interpreter, sexpr, minLength, buf, "", passAll);
}

pub fn expectLongList(interpreter: *Interpreter, sexpr: SExpr, minLength: usize, maxLength: usize) Result!usize {
    return expectLongListOf(interpreter, sexpr, minLength, maxLength, "", passAll);
}

fn alistValue(at: *const Source.Attr, symbol: []const u8, itemData: anytype) Result!SExpr {
    const ItemT = @TypeOf(itemData);
    switch (@typeInfo(ItemT)) {
        .@"type" => {
            const T = itemData;
            if (std.meta.hasFn(T, "init")) {
                return try T.init(at);
            } else if (std.meta.hasFn(T, "fun")) {
                return try SExpr.Builtin(at, symbol, T.fun);
            }
        },
        .@"fn" => |fun| {
            switch (fun.params.len) {
                1 => return try itemData(at),
                3 => return try SExpr.Builtin(at, symbol, itemData),
                else => {},
            }
        },
        else => if (ItemT == SExpr) {
            return itemData;
        },
    }
    @compileLog("unsupported value", itemData);
    @compileError("unsupported value type in alistBuilder");
}

pub fn alistHelper(comptime T: type, at: *const Source.Attr, list: anytype, tail: T, ctx: anytype, comptime callback: fn (@TypeOf(ctx), *const Source.Attr, SExpr, SExpr, T) Result!T) Result!T {
    var frame = tail;

    inline for (std.meta.fields(@TypeOf(list))) |field| {
        const item = @field(list, field.name);
        const valueIndex = comptime if (item.len == 2) 1 else 2;
        if (comptime TypeUtils.isString(@TypeOf(item[0]))) {
            const symbol = try SExpr.Symbol(at, item[0]);
            const value = try alistValue(at, symbol.forceSymbolSlice(), item[valueIndex]);

            frame = try callback(ctx, at, symbol, value, frame);
        } else if (comptime TypeUtils.isTuple(@TypeOf(item[0]))) {
            const primaryName = item[0][0];
            const value = try alistValue(at, primaryName, item[valueIndex]);

            inline for (std.meta.fields(@TypeOf(item[0]))) |nameField| {
                const nameItem = @field(item[0], nameField.name);
                const symbol = try SExpr.Symbol(at, nameItem);

                frame = try callback(ctx, at, symbol, value, frame);
            }
        } else {
            @compileLog("unsupported key type", item[0]);
            @compileError("unsupported key type in alistBuilder");
        }
    }

    return frame;
}

pub fn alistBuilder(attr: *const Source.Attr, list: anytype) Result!SExpr {
    return alistHelper(SExpr, attr, list, try SExpr.Nil(attr), {}, struct {
        fn fun (_: void, at: *const Source.Attr, symbol: SExpr, value: SExpr, frame: SExpr) Result!SExpr {
            return SExpr.Cons(at, try SExpr.Cons(at, symbol, value), frame);
        }
    }.fun);
}

pub fn bindBuiltinEnv(interpreter: *Interpreter, outputEnv: SExpr, builtinEnv: Builtin.EnvName) Result!void {
    inline for (comptime std.meta.fieldNames(Builtin.EnvName)) |builtinName| {
        if (@field(Builtin.EnvName, builtinName) == builtinEnv) {
            return try interpreter.bindCustomEnv(outputEnv, @field(Builtin.Envs, builtinName));
        }
    }
}

pub fn bindCustomEnv(interpreter: *Interpreter, outputEnv: SExpr, customEnv: anytype) Result!void {
    try alistHelper(void, interpreter.context.attr, customEnv, {}, outputEnv, struct {
        fn fun (env: SExpr, at: *const Source.Attr, symbol: SExpr, value: SExpr, _: void) Result!void {
            return extendEnvFrame(at, symbol, value, env);
        }
    }.fun);
}
