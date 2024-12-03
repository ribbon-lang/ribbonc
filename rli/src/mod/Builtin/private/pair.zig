const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

pub const Doc =
    \\This module contains functions for creating and manipulating pairs.
    \\
    ;

pub const Env = .{
    .{ "pair/cons", "join a head and tail into a pair", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            return try SExpr.Cons(at, rArgs[0], rArgs[1]);
        }
    } },
    .{ "pair/car", "get the head of a pair", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const list = try interpreter.eval1(args);
            return (try interpreter.castPair(at, list)).car;
        }
    } },
    .{ "pair/set-car!", "set the head of a pair; returns the old value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            const list = try interpreter.castPair(at, rArgs[0]);
            const newCar = rArgs[1];
            const oldCar = list.car;
            list.car = newCar;
            return oldCar;
        }
    } },
    .{ "pair/cdr", "get the tail of a pair", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const list = try interpreter.eval1(args);
            return (try interpreter.castPair(at, list)).cdr;
        }
    } },
    .{ "pair/set-cdr!", "set the tail of a pair; returns the old value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            const list = try interpreter.castPair(at, rArgs[0]);
            const newCdr = rArgs[1];
            const oldCdr = list.cdr;
            list.cdr = newCdr;
            return oldCdr;
        }
    } },
};
