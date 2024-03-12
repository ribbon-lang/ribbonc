#!/usr/env/bin ribboni

Llama = namespace
    use ../Foo/{ bar
               , ../tuple
               , baz as (0) ` qux `
               , box/..
               , qubert/.. hiding llama
               , foo/.. hiding {filament, fiber}
               , module test
               , ..
               } as Quux

    bar = with Console
            printLn | msg => continue ()
        do foo(1, 2)

    thisShouldFail

test = namespace

test2 = namespace
    ;; intentionally empty

foo = effect
    ;; intentially empty

axx = effect
    expectAFailInAxx

qubert = effect a =>
    ` qube ` : a -> a

Semigroup = class a =>
    ` <> ` : a -> a -> a

Monoid = class a where Semigroup a =>
    mempty : a

Monad = class m where Applicative m =>
    ` >>= ` : m a -> (a -> m b) -> m b

MonadTrans = class t : Type -> (Type -> Type) -> Type -> Type =>
    lift : where Monad m => m a -> t m a

AssociatedTypeTest = class x =>
    AssocType : type
    AssocTypeQuant1 : type a
    AssocTypeQuant2 : type a, b : Int
    AssocTypeQual : type where FooBar AssocTypeQual
    AssocTypeFull : type a where LlamaDuck a

InstanceTest = class x =>
    Lifted : type x
    lift : x -> Lifted x

Int'InstanceTest = instance a for Maybe a =>
    Lifted = type Maybe a
    lift = fun x => +/Just x

TestResult1 = type a => Result TestFailure a
TestResult2 = type
    Result TestFailure
ExpectTypeFail = type

MyTuple =
    struct a, b => a, b

MyStruct
    = struct a, b =>
        x : a,
        y : b

MyUnion
    =
        union a =>
            99 \ foo : a,
            tumadre : (),

id
    : forall a => a -> a
    = fun x => x

decrement : Int -> Int
    = fun x => x - 1

increment : Int -> Int = fun x => x + 1

double
    : Float -> Float
        = fun x => x * 2.0

externalThing : (Float, Float) -> Float
