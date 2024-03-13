foo : (Int, Int) -> Int in {Console}
    = fun x, y =>
        printLn "foo"
        x + y

bar : Int
    = with Console
        printLn | msg => continue ()
    do foo(1, 2)

id
    : forall a => a -> a
    = fun x => x
