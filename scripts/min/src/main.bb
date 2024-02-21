;; comment

pub Foo = type Int
use core/`/`
use ../bar
use module test
use module test/foo/..
use ../..
use {}
use ..
use {abc, ..}
use ../{test}
use {test}
use {test, ../foo/bar/baz}
use ../{../../baz/.., qux/.., ..} as Qux

lits = 'x'; '\x00'; "\"abc\""; "\0foo"; 1; 1.0


Baz = namespace
    type infixl 10 ~ = whatever
    infix + : (Int, Int) -> Int = whatever

Bar = effect
    bar : Int -> Int

Read = effect a =>
    read : () -> a

Write = effect
    write : 'a -> ()

Wem = type
    [Read 'a, Write 'a]

foo = fun x =>
    x + 1

bar =
    fun y =>
        y - 1

xyz : Int -> Int

value atom baz : Int
    = foo (bar 5)
