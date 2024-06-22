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
language with a focus on data polymorphism and structured allocation, motivated
by deep extensibility in the style of Lua/LISP

This project is still in the very early development stages. The compiler created
here will target bytecode for
[Ribbon<sup>I</sup>](https://github.com/ribbon-lang/ribboni), an effect-aware
low level stack machine interpreter (which is also in a very early stage of
development)

For now, issues are turned off and pull requests without prior
[discussion](#discussion) are discouraged.


## Roadmap

Project is currently undergoing a redesign behind the scenes. Public updates *soon!*


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
