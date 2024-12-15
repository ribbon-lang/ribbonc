local foo = fun(x) {
    print-ln x
    + x 1
}

print-ln "woo " (foo 1)
print-ln (not true)
print-ln 'foo


import String

print-ln (String/length "test")

print-ln `(1 2 3)
print-ln `(1 2 ,(foo 1))
print-ln `(1 2 ,@'(3 4))
