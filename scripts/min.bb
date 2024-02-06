type Foo = Int

infix + = whatever

effect Bar =
    bar : Int -> Int

foo = fun x =>
    x + 1

bar =
    fun y =>
        y - 1

baz = foo (bar 5)
