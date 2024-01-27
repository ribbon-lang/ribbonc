<div align="center">
  <img style="height: 18em"
       alt="Ribbon Language Logo"
       src="https://ribbon-lang.github.io/images/logo_full.svg"
       />
</div>

<div align="right">
  <h1>Ribbon<sup>C</sup></h1>
  <h3>The Ribbon Language Compiler</h3>
  (Bootstrap Edition)
</div>

-----

This is a minimal implementation of Ribbon, an embeddable algebraic effects
language with a focus on data polymorphism and allocator strategies, motivated
by deep extensibility in the style of Lua/LISP.

This project is still in the very early development stages. The compiler created
here will target bytecode for
[Ribbon<sup>I</sup>](https://github.com/ribbon-lang/ribboni), an effect-aware low level
stack machine interpreter (which is also in a very early stage of development).

For now, issues are turned off and pull requests without prior
[discussion](#discussion) are discouraged. See [Methodology](#methodology) for
some exposition on this situation

#### Contents
- [Roadmap](#roadmap)
- [Methodology](#methodology)
- [Dependencies](#dependencies)
- [Usage](#usage)
- [Discussion](#discussion)
- [References](#references)


## Roadmap

#### Implementation progress
- [X] Lexical analysis
- [ ] Syntactic analysis (parsing)
- [ ] Semantic analysis (graph of module, etc)
- [ ] Type inference
- [ ] Code generation
- [ ] Driver CLI

#### Planned features
Note this is not a full list for the final language, only the bootstrap
- Polymorphic top-level variable definitions
- Pattern matching
- Full, reliable type inference for all features
- User-defined effect types
- Lexically scoped effect handlers
- Row-polymorphic structural data types (product and sum)
- Row-polymorphic effect annotations for functions
- Monoidal concatenation constraints for row types
- Subtyping constraints for row types
- Lexically scoped variables for lambda functions, using hidden effects
- Simple built-ins like map, set

#### Non-goals
Some things I do not plan to do, which again apply to the bootstrap only
- Creating a reference implementation for the entire language
- First-class closures
- High performance / optimization
- Great errors à la rustc
- The vast majority of quality of life concerns


## Methodology

At some point I will revise the issue/pr policy. The reason for the current
situation is that I have not really defined a specification, and things are
moving fast right now.

The basic plan is to immediately bootstrap the language, and create a
self-hosted compiler that runs within the bytecode interpreter itself. I may
wait until the bootstrapping phase to open up to other contributors, but in the
unlikely event someone expresses interest in helping out meantime, I might take
the time to nail down a small spec and provide some issues for pr guidance.

In terms of code, this is just a basic Haskell library and driver application,
attempting to implement the language with as little cognitive overhead as
possible. There are relatively few dependencies, and I try to keep the
implementation reader-friendly.

To this end I try to maintain some good practices such as providing informative
commit messages, instituting good modularity, keeping files to 80 columns,
consistently indenting and styling blocks, avoiding obfuscating programming
patterns like super-dense point free notation inside monads, etc.

I am documenting everything as well. This is mostly to help maintain my own
sanity as the project grows, so it may be a bit minimal in terms of
instructional quality for the uninitiated. If you have any questions about the
implementation, feel free to [ask](#discussion).

I have included `mtl` in the dependencies, but mostly for its typeclasses rather
than its transformer data types. Instead I prefer to write monad definitions out
as newtypes, as it helps me to reason about them while things are still
evolving. Some folks may find this annoying, but I hope they will at least agree
it is fairly inconsequential in broader the scale of the project

In addition to `mtl`, I rely on `containers` for basic things like Map, but
beyond that I intend to implement most support libraries myself. These support
libraries include things like pretty printing (`Ribbon.Display`) and a parser
monad (`Ribbon.Syntax.ParserM`). There are great libraries on stackage for both
of these, but I have opted to include implementations to provide a good
reference to the reader (ie, my future self during bootstrap)

I am using a few GHC extensions, mostly involving typeclass behaviors and basic
syntax quality of life enhancements such as block arguments.

Where ever possible, I am pulling code (or at least inspiration) from my prior
work. Some of this is available on GitHub already, if you would like a
sneak-peak of what's to come. For example, a rough prototype of the type
inference is available [here](https://github.com/noxabellus/monoidal-rows)

Additionally, I have referenced some great publications in the process of design
and planning for this language, do take a look at the [References](#references)
section!

The code here and within all other Ribbon libraries are and will remain licensed
under [Apache 2.0](LICENSE)


## Dependencies

For a full list see [package.yaml](package.yaml), but in short, just the basic
boilerplate dependencies such as `containers`, `mtl`, etc

The Stack configuration is locked to `lts-21.25`, using GHC `9.4.8`


## Usage

The driver itself will not be implemented for some time. If you would like to
play with features implemented thus far, the best way to do that is via
`stack repl`

Currently you can:
- Read files as source-tracking File objects, using `loadFile`
- Play with the lexer, using `lexFile`/`lexString`


## Discussion

Eventually I will create some places for public discourse about the language,
for now you can reach me via:
- Discord DM, or on various dev servers: my username is `noxabellus`
- Email: noxabellus@gmail.com


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
- [Thunderseethe's blog](https://thunderseethe.dev/posts/type-inference/)
- [Ryan Brewer's blog](https://ryanbrewer.dev/posts/safe-mmm-with-coeffects.html)
- [Colin James' blog](https://compiler.club)