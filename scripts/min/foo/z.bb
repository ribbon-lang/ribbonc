module "test"
    version 1.0.0
    author "noxabellus"
    license "MIT"
    sources "./src", "./tests"
    dependencies
        "core" @ 1.0.0
        "tag" @ 0.1.0

pub quux = namespace
    use module core/{list/{./, List}, ops/{+, -, infix < as lt}}
    use module tag/{./, Tag}

    Semigroup = class a =>
        infixl 9 <> : (a, a) -> a

    instance t, a where Semigroup t, Semigroup a
    for Semigroup (Tag t a) =>
        <> = tag/concat

    Vec2 = struct a =>
        x : a
        y : a

    Maybe = union a =>
        Just : a
        Nothing

    Read = effect a =>
        read : () -> a

    Write = effect a =>
        write : a -> ()

    Mem = type a => [Read a, Write a]

    fib : forall n where Num n => n -> n
        = fun n => if lt(n, 2)
            then n
            else fib (n - 1) + fib (n - 2)


