<!--%src/mod/Builtin-->

<div align="center">
  <img style="height: 18em"
       alt="Ribbon Language Logo"
       src="https://ribbon-lang.github.io/images/logo_full.svg"
       />
</div>

<div align="right">
  <h1>Ribbon<sup>C</sup></h1>
  <h3>The Ribbon Language Compiler</h3>
  <sup><!--#Readme:Build version--></sup>
</div>

---

This is a compiler implementation for the Ribbon programming language. This
project is still in the very early development stages. For now, issues are
turned off and pull requests without prior [discussion](#discussion) are
discouraged.


## Contents

+ [What is Ribbon?](#what-is-ribbon)
+ [Roadmap](#roadmap)
    - [Todo for v0.0.0 release](#todo-for-v000-release)
+ [Discussion](#discussion)
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
+ [References](#references)


## What is Ribbon?

Ribbon aims to strike a new balance between high level programmer experience and
low level systems access. It is an algebraic effects language in the lineage of
[Koka](https://koka-lang.github.io/) and [Effekt](https://effekt-lang.org/), but
with some adapted influences from the imperative world inspired by a history in game
development, particularly in its focus on performance and allocation, and the
capability to be embedded into other applications.
Semantically, Ribbon has a focus on (fully inferred) strong
and static data types, structural data polymorphism, and allocator strategies.
The design is also motivated by a pursuit of deep extensibility in the style of
[Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) and
[Terra](https://terralang.org/).

The compiler created here will target bytecode for
[Ribbon<sup>I</sup>](https://github.com/ribbon-lang/ribboni), an effect-aware
low level stack machine interpreter (which is in a very early stage of
development) with JIT capabilities. Eventually, the two will be merged into a
single unified system, and AOT native compilation will be supported as well.

## Roadmap

+ ✅ Lisp interpreter (95%)
+ ✅ Lisp environment (95%)
+ ✅ REPL (95%)
+ ✅ CLI (95%)
+ 🟥 Type inference (0% here, but [well prototyped](https://github.com/noxabellus/monoidal-rows))
+ 🟥 Bytecode generation (0%)
+ 🟥 ML-like syntax via lisp reader macros (0%)

The compiler is undergoing heavy development, but there will be test releases
available for most platforms soon. The release will consist of a c header file,
a static library, and a REPL/CLI application. Features implemented thus far
constitute a lisp interpreter with an effects system and a fairly complete
standard library environment.

#### Todo for v0.0.0 release:
+ Finish integrating existing global environments with the new module system
+ Comment syntax
+ Support reader macros
+ Ensure a minimum REPL functionality in Windows terminals
+ Wrap more API with C bindings
+ More testing & clean up


## Discussion

Eventually I will create some places for public discourse about the language,
for now you can reach me via:
- Email: noxabellus@gmail.com
- Discord DM, or on various dev servers: my username is `noxabellus`


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

The `ribbonc` executable is a work in progress, but offers a functional command line interface for Ribbon.

#### CLI Usage
<!--#Readme:CLI usage-->

#### CLI Options
<!--#Readme:CLI options-->


### REPL

The `ribbonc` executable is a work in progress, but offers a functional REPL interface for Ribbon.

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
module.addImport("RibbonC", ribbon_c.module("Core"));
```
3. See [`src/bin/ribbonc.zig`](src/bin/ribbonc.zig) for usage

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


## References

We truly stand on the shoulders of giants and I'm afraid listing everything I've
referenced in the design of this language would be impossible, so the following
are some highlights I found particularly well-written, informative, or otherwise
inspiring

- [Abstracting Extensible Data Types or Rows By Any Other Name](https://dl.acm.org/doi/10.1145/3290325)
- [A Polymorphic Record Calculus and Its Compilation](https://dl.acm.org/doi/10.1145/218570.218572)
- [Generalized Evidence Passing for Effect Handlers](https://dl.acm.org/doi/10.1145/3473576)
- [Do Be Do Be Do](https://dl.acm.org/doi/10.1145/3009837.3009897)
- [Zero-cost Effect Handlers by Staging](https://se.informatik.uni-tuebingen.de/publications/schuster19zero.pdf)
- [Typed Memory Management in a Calculus of Capabilities](https://dl.acm.org/doi/10.1145/292540.292564)
- [A Lightweight Formalism for Reference Lifetimes and Borrowing in Rust](https://dl.acm.org/doi/10.1145/3443420)
- [Alias Types for Recursive Data Structures](https://dl.acm.org/doi/10.5555/867133)
- [System F-omega with Equirecursive Types for Datatype-Generic Programming](https://dl.acm.org/doi/10.1145/2837614.2837660)
- [Numbering Matters: First-Order Canonical Forms for Second-Order Recursive](https://dl.acm.org/doi/10.1145/1016848.1016872)
- [The Simple Essence of Algebraic Subtyping](https://dl.acm.org/doi/10.1145/3409006)
- Basically every publication from the fabulous
[Daan Leijen](https://www.microsoft.com/en-us/research/people/daan/publications/)
- [How to compile pattern matching](https://julesjacobs.com/notes/patternmatching/patternmatching.pdf)
and various other works by [Jules Jacobs](https://julesjacobs.com/)
- [Lisp in Small Pieces](https://www.cambridge.org/core/books/lisp-in-small-pieces/66FD2BE3EDDDC68CA87D652C82CF849E)
- [Let Over Lambda](https://letoverlambda.com/index.cl/toc)
- [Fear of Macros](https://www.greghendershott.com/fear-of-macros/all.html)
- [Thunderseethe's blog](https://thunderseethe.dev/posts/type-inference/)
- [Ryan Brewer's blog](https://ryanbrewer.dev/posts/safe-mmm-with-coeffects.html)
- [Colin James' blog](https://compiler.club)
