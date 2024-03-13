;; comment

pub Foo = type Int
use core/`/`
use ../bar
use module test
use module test/foo/..
use ../..
;; use {} FIXME
use ..
use {abc, ..}
use ../{test1}
use {test2}
use {test3, ../foo/bar/baz}
use ../{../../baz/.., qux/.., ..} as Qux

lits = 'x'; '\x00'; "\"abc\""; "\0foo"; 1; 1.0


Baz = namespace
    ` ~ ` 10 = type whatever
    (10) ` + ` : (Int, Int) -> Int = whatever

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

baz : Int
    = foo (bar 5)
