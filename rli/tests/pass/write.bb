(def fun test-function (msg)
    (write-file std-out (string-intercalate " " "hello" msg "\n")))

(test-function "world")
