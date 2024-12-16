local check = fun({* args}) print-ln args

check 1 2 3 4 5
check 1 2
check 1
check

local check2 = fun
    ({+ args}) print-ln args
    else print-ln "no args"

check2 1 2 3 4 5
check2 1 2
check2 1
check2

local foo = fun(x)
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
