<!--%src/mod/Builtin-->

<div align="center">
  <img style="height: 18em"
       alt="Ribbon Language Logo"
       src="https://ribbon-lang.github.io/images/logo_full.svg"
       />
</div>

<div align="right">
  <h1>rli</h1>
  <h3>The Ribbon Lisp Interpreter</h3>
  <sup><!--#Readme:Build version--></sup>
</div>

---

This is the intermediate representation for the
[Ribbon](https://ribbon-lang.github.io) programming language.

## Contents

+ [Usage](#usage)
    - [Building from source](#building-from-source)
        * [Zig Build Commands](#zig-build-commands)
        * [Zig Build Options](#zig-build-options)
    - [CLI](#cli)
        * [CLI Usage](#cli-usage)
        * [CLI Options](#cli-options)
    - [REPL](#repl)
        * [REPL Commands](#repl-commands)
    - [Inclusion as a library](#inclusion-as-a-library)
        * [From Zig](#from-zig)
        * [From C](#from-c)
        * [From other languages](#from-other-languages)
+ [Lisp dialect](#lisp-dialect)
    - [Syntax](#syntax)
    - [Environment](#environment)
        <!--#Readme:Builtin toc -->


## Usage

### Building from source
You will need [`zig`](https://ziglang.org/); likely, the nightly build.
The latest version known to work is `<!--#Readme:Build zig-version-->`.

You can either:
+ Get it through [ZVM](https://www.zvm.app/) or [Zigup](https://marler8997.github.io/zigup/) (Recommended)
+ [Download it directly](https://ziglang.org/download)
+ Get the nightly build through a script like [night.zig](https://github.com/jsomedon/night.zig/)

#### Zig Build Commands
There are several commands available for `zig build` that can be run in usual fashion (i.e. `zig build run`):
<!--#Readme:Build commands-->

Running `zig build` alone will build with the designated or default target and optimization levels.

See `zig build --help` for more information.

#### Zig Build Options
In addition to typical zig build options, the build script supports the following options (though not all apply to every step):
<!--#Readme:Build options-->

See `zig build --help` for more information.


### CLI

The `rli` executable is a work in progress, but offers a functional command line interface for Ribbon.

#### CLI Usage
<!--#Readme:CLI usage-->

#### CLI Options
<!--#Readme:CLI options-->


### REPL

The `rli` executable is a work in progress, but offers a functional REPL interface for Ribbon.

See [CLI](#cli) for information on how to access the REPL mode.

#### REPL Commands
<!--#Readme:CLI commands-->


### Inclusion as a library

#### From Zig

1. Include ribbon in your `build.zig.zon` in the `.dependencies` section,
   either by linking the tar, `zig fetch`, or provide a local path to the source.
2. Add ribbon to your module imports like this:
```zig
const ribbon_c = b.dependency("ribbon-c", .{
    // these should always be passed to ensure ribbon is built correctly
    .target = target,
    .optimize = optimize,

    // additional options can be passed here, these are the same as the build options
    // i.e.
    // .logLevel = .info,
});
module.addImport("rli", ribbon_c.module("Core"));
```
3. See [`src/bin/rli.zig`](src/bin/rli.zig) for usage

#### From C

Should be straight forward, though the API is limited as of now.
Use the included header file, then link your program with the `.lib`/`.a` file.

Example of binding from C can be found at [`tests/test.c`](tests/test.c),
and at [`tests/test.sh`](tests/test.sh)

#### From other languages

If your host language has C FFI, it should be fairly straight forward.
If you make a binding for another language,
please [let me know](#discussion) and I will link it here.


## Lisp dialect

Mostly the same as other lisps, major differences as of now include:
+ No comments yet
+ Only decimal integer literals are supported as of now
+ Character literals are c-style (i.e. `'c'`, `'\x00'` etc)
+ Quasiquotes do not traverse through regular quotes; this is used to allow nested quasi logic
+ Some naming convention differences (e.g. `out<-in` conversion function names)
+ Minimal environment

### Syntax

- Nil `()`
- Symbols `foo`, `foo'`, `/`, `+inf`
- Integers `1001`, `+9`, `-32`
- Characters `'x'`, `'\x00'`, `'\esc'`, `'\t'`, `'\''`
- Floats `1.0`, `1.`, `.0`, `+1.0e-3`
- Strings `"foo"`, `"\tfoo\x00"`, `"\""`
- Pairs `(1 . 2)`, `(1 2 3 . 4)`
- Lists `(1 2 3)`, `(+ 1 2 3)`
- Quotes `'foo`, `'(1 2 3)`
- Quasiquotes
    ```
    `foo
    `(1 2 3)
    ```
- Unquote
    ```
    `(1 ,foo 3)
    ```
- Unquote splicing
    ```
    `(1 2 ,@foo)
    ```

### Environment

<!--#Readme:Builtin env-->
