#!/usr/env/bin ribboni

Llama = namespace
    use ../Foo/{ bar
               , ../tuple
               , baz as (0) ` qux `
               , box/..
               , qubert/.. hiding llama
               , foo/.. hiding {a, b}
               , module test
               , ..
               } as Quux

    bar = with Console
            printLn | msg => continue ()
        do foo(1, 2)

    qubert = effect a =>
        ` qube ` : a -> a
