## TODO

* prevent infinite recursion when printing self-referential structures

* add dir traversal and stat functionality to IO

* ensure minimal functionality under windows native terminal repl

* make pattern matching tools and eval take a diagnostic structure instead of the current rich error facility

* audit source attribution consistency in Eval

* consider adding stateful variants of builtins and extern functions
    - this would enable a cleaner interface for implementing the current effect handler return semantics

* replace `VMIN` and `VTIME` in [REPL](src/mod/REPL.zig#L526) with `std.posix` values
    - blocked by the fact `std.posix` does not expose these values

* investigate how non-gc allocators' lack of finalizers affects execution of ribbon code,
  when using them as the backing storage of Context
