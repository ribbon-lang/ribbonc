;; comment

pub type Foo = Int

use ../bar
use module test
use module test.foo..
use ../..
use {}
use ..
use {abc, ..}
use ../{test}
use {test}
use {test, ../foo}
use ./{../../baz.., qux..} as Qux

lits = 'x'; '\x00'; "\"abc\""; "\0foo"; 1; 1.0


namespace Baz =
    type infixl 10 ~ = whatever
    infix + : (Int, Int) -> Int = whatever

effect Bar =
    bar : Int -> Int

effect Read =
    read : () -> 'a

effect Write =
    write : 'a -> ()

type Mem =
    [Read 'a, Write 'a]

foo = fun x =>
    x + 1

bar =
    fun y =>
        y - 1

value xyz : Int -> Int

value atom baz : Int
    = foo (bar 5)
