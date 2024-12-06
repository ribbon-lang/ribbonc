(import io)
(import string)

(def fun test-function (msg)
    (io/write-file io/std-out (string/intercalate " " "hello" msg "\n")))

(test-function "world")
