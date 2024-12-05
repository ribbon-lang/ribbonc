## TODO

* module import via file path

* add dir traversal and stat functionality to IO

* ensure minimal functionality under windows native terminal repl

* make lambda list tools and eval take a diagnostic structure instead of the current rich error facility

* audit source attribution consistency in Eval
    - in many places in builtin, we were are attributing errors to the place the value was created,
      when in fact that will not show where the error actually occurred

* write a pass and/or fail test for each builtin module

* consider adding stateful variants of builtins and extern functions
    - this would enable a cleaner interface for implementing the current effect handler return semantics

* support c-tests on other hosts

* replace `VMIN` and `VTIME` in [REPL](src/mod/REPL.zig#L526) with `std.posix` values
    - blocked by the fact `std.posix` does not expose these values

* investigate how non-gc allocators' lack of finalizers affects execution of ribbon code,
  when using them as the backing storage of Context

* prevent infinite recursion when printing self-referential structures

* consider moving zg inside of zig-text-utils, enabling it to be used by build scripts
