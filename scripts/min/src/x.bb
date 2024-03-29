#!/usr/env/bin ribboni

namespace Llama =
    use ~/test
    use file "foo.bb"
    use ./Quux/test as ` Qux `
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

namespace test
namespace test1 =

namespace test2 =
    ;; intentionally empty

effect foo =
    ;; intentially empty

effect axx =
    expectAFailInAxx

effect qubert a =
    ` qube ` : a -> a

class Semigroup a =
    ` <> ` : a -> a -> a

class Monoid a where Semigroup a =
    mempty : a

class Monad m where Applicative m =
    ` >>= ` : m a -> (a -> m b) -> m b

class MonadTrans t : Type -> (Type -> Type) -> Type -> Type =
    lift : where Monad m => m a -> t m a

class AssociatedTypeTest x =
    type AssocType
    type AssocTypeQuant1 a
    type AssocTypeQuant2 a, b : Int
    type AssocTypeQual
        where FooBar AssocTypeQual
    type AssocTypeFull a where LlamaDuck a

class InstanceTest x =
    type Lifted x
    lift : x -> Lifted x

instance InstanceTest'Int a for Maybe a =
    type Lifted = Maybe a
    lift = fun x => +/Just x

type TestResult1 a = Result TestFailure a
type TestResult2 =
    Result TestFailure
type ExpectTypeFail1 =
type ExpectTypeFail2

struct MyTuple a, b = a, b

struct MyStruct a, b =
    x : a,
    y : b

union MyUnion a =
    99 \ foo : a,
    tumadre : (),

id
    : for a => a -> a
    = fun x => x

decrement : Int -> Int
    = fun x => x - 1

increment : Int -> Int = fun x => x + 1

double
    : Float -> Float
        = fun x => x * 2.0

externalThing : (Float, Float) -> Float
