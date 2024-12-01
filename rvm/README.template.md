<div align="left">
  <img style="height: 10em"
       alt="Ribbon Language Logo"
       src="https://ribbon-lang.github.io/images/logo_full.svg"
       />
</div>

<div align="right">
  <h1>rvm</h1>
  <h3>The Ribbon Virtual Machine</h3>
  <sup><!--#Readme:Build version--></sup>
</div>

---

This is a virtual machine implementation for the
[Ribbon](https://ribbon-lang.github.io) programming language.

For the bytecode ISA specification, see [rbc-isa](https://github.com/ribbon-lang/rbc-isa)

For the bytecode implementation, see [rbc](https://github.com/ribbon-lang/rbc)


## Contents

+ [Roadmap](#roadmap)
    - [Todo for v0.1.0 release](#todo-for-v010-release)
+ [Discussion](#discussion)
+ [Usage](#usage)
    - [Building from source](#building-from-source)
        * [Zig Build Commands](#zig-build-commands)
        * [Zig Build Options](#zig-build-options)
    - [Inclusion as a library](#inclusion-as-a-library)
        * [From Zig](#from-zig)
        * [From C](#from-c)
        * [From other languages](#from-other-languages)
    - [CLI](#cli)

## Roadmap

+ ✅ Bytecode interpreter (90%)
+ 🟥 CLI (0%)
+ 🟥 C Api (0%)

Initial development of the interpreter itself is essentially complete, aside
from the foreign function interface. Some bug squashing is likely still
necessary, of course, as testing is minimal at this time. C-Api and CLI are
non-existent right now.

#### Todo for v0.1.0 release:
+ Foreign function interface
+ Implement CLI
+ Implement C Api
+ More testing and cleanup


## Discussion

Eventually I will create some places for public discourse about the language,
for now you can reach me via:
- Email: noxabellus@gmail.com
- Discord DM, or on various dev servers: my username is `noxabellus`
- For `rvm`-specific inquiries, feel free to create an issue on this repository


## Usage

### Building from source

You will need [`zig`](https://ziglang.org/); likely, a nightly build.
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

### Inclusion as a library

#### From Zig

1. Include Rvm in your `build.zig.zon` in the `.dependencies` section,
   either by linking the tar, `zig fetch`, or provide a local path to the source.
2. Add Rvm to your module imports like this:
```zig
const rvm = b.dependency("rvm", .{
    // these should always be passed to ensure ribbon is built correctly
    .target = target,
    .optimize = optimize,

    // additional options can be passed here, these are the same as the build options
    // i.e.
    // .logLevel = .info,
});
module.addImport("Rvm", rvm.module("Core"));
```
3. See [`src/bin/rvm.zig`](src/bin/rvm.zig) for usage

#### From C

Should be straight forward, when the API is in place. Current status: 0%

Use the included header file, then link your program with the `.lib`/`.a` file.

#### From other languages

If your host language has C FFI, it should be fairly straight forward. If you make a binding for another language, please [let me know](#discussion) and I will link it here.


### CLI

The `rvm` executable is a work in progress.

#### CLI Usage
<!--#Readme:CLI usage-->

#### CLI Options
<!--#Readme:CLI options-->
