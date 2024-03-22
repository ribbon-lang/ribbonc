pub foo = 1 + 1

pub namespace quux =
    use module core/{list/{./, List}, ops/{` + `, ` - `, ` < ` as lt}}
    use module tag/{./, Tag}

    class Semigroup a =
        9 ` <> ` : (a, a) -> a

    instance Semigroup'Tag t, a
        where Semigroup t, Semigroup a
        for Semigroup (Tag t a) =>
            ` <> ` = tag/concat

    struct Vec2 a =
        x : a
        y : a

    union Maybe a =
        Just : a
        Nothing : ()

    union Read a =
        read : () -> a

    union Write a =
        write : a -> ()

    type Mem a = [Read a, Write a]

    fib : forall n where Num n => n -> n
        = fun n => if lt(n, 2)
            then n
            else fib (n - 1) + fib (n - 2)


