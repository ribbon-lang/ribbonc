const std = @import("std");

const Rml = @import("root.zig");
const Obj = Rml.Obj;
const Result = Rml.Result;
const Object = Rml.Object;
const Origin = Rml.Origin;
const Nil = Rml.Nil;
const Int = Rml.Int;
const Float = Rml.Float;
const Char = Rml.Char;
const Writer = Rml.Writer;
const Interpreter = Rml.Interpreter;
const TypeId = Rml.TypeId;
const castObj = Rml.castObj;


/// Print any number of arguments followed by a new line
pub fn @"print-ln"(interpreter: Obj(Interpreter), callOrigin: Origin, args: []const Object) Result! Object {
    const rml = interpreter.getRml();

    const stdout = std.io.getStdOut();
    const nativeWriter = stdout.writer();

    const writer: Obj(Writer) = try .init(rml, callOrigin, .{nativeWriter.any()});
    defer writer.deinit();

    try writer.data.print("{}: ", .{callOrigin});

    for (args) |arg| try arg.getHeader().onFormat(writer);

    try writer.data.writeAll("\n");

    const nil: Obj(Nil) = try .init(rml, callOrigin);
    defer nil.deinit();

    return nil.typeErase();
}



/// Print any number of arguments
pub fn print(interpreter: Obj(Interpreter), callOrigin: Origin, args: []const Object) Result! Object {
    const rml = interpreter.getRml();

    const stdout = std.io.getStdOut();
    const nativeWriter = stdout.writer();

    const writer: Obj(Writer) = try .init(rml, callOrigin, .{nativeWriter.any()});
    defer writer.deinit();

    for (args) |arg| try arg.getHeader().onFormat(writer);

    const nil: Obj(Nil) = try .init(rml, callOrigin);
    defer nil.deinit();

    return nil.typeErase();
}



/// Alias for `+`
pub const add = @"+";
/// Sum any number of arguments of type `int | float | char`;
/// if only one argument is provided, return the argument's absolute value
pub fn @"+"(interpreter: Obj(Interpreter), callOrigin: Origin, args: []const Object) Result! Object {
    if (args.len == 0) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 1 argument, found 0", .{});

    var sum: Object = args[0].clone();
    defer sum.deinit();

    if (args.len == 1) {
        if (castObj(Int, sum)) |int| {
            defer int.deinit();

            const result: Obj(Int) = try .wrap(int.getRml(), callOrigin, @intCast(@abs(int.data.*)));
            defer result.deinit();

            return result.typeErase();
        } else if (castObj(Float, sum)) |float| {
            defer float.deinit();

            const result: Obj(Float) = try .wrap(float.getRml(), callOrigin, @abs(float.data.*));
            defer result.deinit();

            return result.typeErase();
        } if (castObj(Char, sum)) |char| {
            defer char.deinit();

            const result: Obj(Char) = try .wrap(char.getRml(), callOrigin, char.data.*);
            defer result.deinit();

            return result.typeErase();
        } else {
            try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char, found {s}", .{TypeId.name(sum.getHeader().type_id)});
        }
    }

    return arithCastReduce(interpreter, callOrigin, &sum, args[1..], struct {
        pub fn int(a: Int, b: Int) Int { return a + b; }
        pub fn float(a: Float, b: Float) Float { return a + b; }
        pub fn char(a: Char, b: Char) Char { return a + b; }
    });
}



/// Alias for `-`
pub const sub = @"-";
/// Subtract any number of arguments of type `int | float | char`;
/// if only one argument is provided, return the argument's negative value
pub fn @"-"(interpreter: Obj(Interpreter), callOrigin: Origin, args: []const Object) Result! Object {
    if (args.len == 0) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 1 argument, found 0", .{});

    var sum: Object = args[0].clone();
    defer sum.deinit();

    if (args.len == 1) {
        if (castObj(Int, sum)) |int| {
            defer int.deinit();

            const result: Obj(Int) = try .wrap(int.getRml(), callOrigin, -int.data.*);
            defer result.deinit();

            return result.typeErase();
        } else if (castObj(Float, sum)) |float| {
            defer float.deinit();

            const result: Obj(Float) = try .wrap(float.getRml(), callOrigin, -float.data.*);
            defer result.deinit();

            return result.typeErase();
        } if (castObj(Char, sum)) |char| { // ???
            defer char.deinit();

            const result: Obj(Char) = try .wrap(char.getRml(), callOrigin, char.data.*);
            defer result.deinit();

            return result.typeErase();
        } else {
            try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char, found {s}", .{TypeId.name(sum.getHeader().type_id)});
        }
    }

    return arithCastReduce(interpreter, callOrigin, &sum, args[1..], struct {
        pub fn int(a: Int, b: Int) Int { return a - b; }
        pub fn float(a: Float, b: Float) Float { return a - b; }
        pub fn char(a: Char, b: Char) Char { return a - b; }
    });
}


/// Alias for `/`
pub const div = @"/";
/// Divide any number of arguments of type `int | float | char`;
/// if only one argument is provided, it is an error
pub fn @"/"(interpreter: Obj(Interpreter), callOrigin: Origin, args: []const Object) Result! Object {
    if (args.len < 2) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 2 arguments, found {}", .{args.len});

    var sum: Object = args[0].clone();
    defer sum.deinit();

    return arithCastReduce(interpreter, callOrigin, &sum, args[1..], struct {
        pub fn int(a: Int, b: Int) Int { return @divFloor(a, b); }
        pub fn float(a: Float, b: Float) Float { return a / b; }
        pub fn char(a: Char, b: Char) Char { return @divFloor(a, b); }
    });
}


/// Alias for `*`
pub const mul = @"*";
/// Multiply any number of arguments of type `int | float | char`;
/// if only one argument is provided, it is an error
pub fn @"*"(interpreter: Obj(Interpreter), callOrigin: Origin, args: []const Object) Result! Object {
    if (args.len < 2) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 2 arguments, found {}", .{args.len});

    var sum: Object = args[0].clone();
    defer sum.deinit();

    return arithCastReduce(interpreter, callOrigin, &sum, args[1..], struct {
        pub fn int(a: Int, b: Int) Int { return a * b; }
        pub fn float(a: Float, b: Float) Float { return a * b; }
        pub fn char(a: Char, b: Char) Char { return a * b; }
    });
}


/// Perform remainder division on any number of arguments of type `int | float | char`;
/// if only one argument is provided, it is an error
pub fn @"rem"(interpreter: Obj(Interpreter), callOrigin: Origin, args: []const Object) Result! Object {
    if (args.len < 2) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 2 arguments, found {}", .{args.len});

    var sum: Object = args[0].clone();
    defer sum.deinit();

    return arithCastReduce(interpreter, callOrigin, &sum, args[1..], struct {
        pub fn int(a: Int, b: Int) Int { return @rem(a, b); }
        pub fn float(a: Float, b: Float) Float { return @rem(a, b); }
        pub fn char(a: Char, b: Char) Char { return @rem(a, b); }
    });
}



fn arithCastReduce(
    interpreter: Obj(Interpreter),
    callOrigin: Origin, acc: *Object, args: []const Object,
    comptime Ops: type,
) Result! Object {
    const offset = 1;
    for (args, 0..) |arg, i| {
        if (castObj(Int, acc.*)) |int| {
            defer int.deinit();

            if (castObj(Int, arg)) |int2| { // int + int = int
                defer int2.deinit();

                const int3: Obj(Int) = try .wrap(int2.getRml(), callOrigin, @field(Ops, "int")(int.data.*, int2.data.*));
                defer int3.deinit();

                acc.deinit();
                acc.* = int3.typeErase();
            } else if (castObj(Float, arg)) |float| { // int + float = float
                defer float.deinit();

                const float2: Obj(Float) = try .wrap(float.getRml(), callOrigin, @field(Ops, "float")(@as(Float, @floatFromInt(int.data.*)), float.data.*));
                defer float2.deinit();

                acc.deinit();
                acc.* = float2.typeErase();
            } else if (castObj(Char, arg)) |char| { // int + char = int
                defer char.deinit();

                const int2: Obj(Int) = try .wrap(char.getRml(), callOrigin, @field(Ops, "int")(int.data.*, @as(Int, @intCast(char.data.*))));
                defer int2.deinit();

                acc.deinit();
                acc.* = int2.typeErase();
            } else {
                try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char for argument {}, found {s}", .{i + offset, TypeId.name(arg.getHeader().type_id)});
            }
        } else if (castObj(Float, acc.*)) |float| {
            defer float.deinit();

            if (castObj(Int, arg)) |int| { // float + int = float
                defer int.deinit();

                const float2: Obj(Float) = try .wrap(int.getRml(), callOrigin, @field(Ops, "float")(float.data.*, @as(Float, @floatFromInt(int.data.*))));
                defer float2.deinit();

                acc.deinit();
                acc.* = float2.typeErase();
            } else if (castObj(Float, arg)) |float2| { // float + float = float
                defer float2.deinit();

                const float3: Obj(Float) = try .wrap(float2.getRml(), callOrigin, @field(Ops, "float")(float.data.*, float2.data.*));
                defer float3.deinit();

                acc.deinit();
                acc.* = float3.typeErase();
            } else if (castObj(Char, arg)) |char| { // float + char = float
                defer char.deinit();

                const float2: Obj(Float) = try .wrap(char.getRml(), callOrigin, @field(Ops, "float")(float.data.*, @as(Float, @floatFromInt(char.data.*))));
                defer float2.deinit();

                acc.deinit();
                acc.* = float2.typeErase();
            } else {
                try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char for argument {}, found {s}", .{i + offset, TypeId.name(arg.getHeader().type_id)});
            }
        } else if (castObj(Char, acc.*)) |char| {
            defer char.deinit();

            if (castObj(Int, arg)) |int| { // char + int = int
                defer int.deinit();

                const int2: Obj(Int) = try .wrap(char.getRml(), callOrigin, @field(Ops, "int")(@as(Int, @intCast(char.data.*)), int.data.*));
                defer int2.deinit();

                acc.deinit();
                acc.* = int2.typeErase();
            } else if (castObj(Float, arg)) |float| { // char, float = float
                defer float.deinit();

                const float2: Obj(Float) = try .wrap(float.getRml(), callOrigin, @field(Ops, "float")(@as(Float, @floatFromInt(char.data.*)), float.data.*));
                defer float2.deinit();

                acc.deinit();
                acc.* = float2.typeErase();
            } else if (castObj(Char, arg)) |char2| { // char, char = char
                defer char2.deinit();

                const char3: Obj(Char) = try .wrap(char2.getRml(), callOrigin, @field(Ops, "char")(char.data.*, char2.data.*));
                defer char3.deinit();

                acc.deinit();
                acc.* = char3.typeErase();
            } else {
                try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char for argument {}, found {s}", .{i + offset, TypeId.name(arg.getHeader().type_id)});
            }
        } else {
            try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char for argument {}, found {s}", .{i, TypeId.name(acc.getHeader().type_id)});
        }
    }

    return acc.clone();
}

