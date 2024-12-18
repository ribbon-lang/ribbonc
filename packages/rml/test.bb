print-ln "hello world"
local check = fun{* args} print-ln args

check 1 2 3 4 5
check 1 2
check 1
(check)

local check2 = fun
    (1 2)
        print-ln "let me see u one, two step"
    {+ args}
        print-ln args
    else
        print-ln "no args"

check2 1 2 3 4 5
check2 1 2
check2 1
check2

local foo = fun x
    print-ln x
    + x 1

print-ln "woo " (foo 1)
print-ln (not true)
print-ln 'foo


import String

print-ln (String/length "test")

print-ln `(1 2 3)
print-ln `(1 2 ,(foo 2))
print-ln `(1 2 ,@'(3 4))


local incrBy = fun x
    fun y (+ x y)
local incr = incrBy 2

print-ln (incr 1)
print-ln (incr 2)
