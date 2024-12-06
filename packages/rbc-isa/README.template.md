<div align="left">
  <img style="height: 10em"
       alt="Ribbon Language Logo"
       src="https://ribbon-lang.github.io/images/logo_full.svg"
       />
</div>

<div align="right">
  <h1>rbc-isa</h1>
  <h3>The Ribbon Bytecode Instruction Set Architecture</h3>
  <sup><!--#ISA version--></sup>
</div>

---

This is an abstract specification of the bytecode instruction set architecture
for the [Ribbon](https://ribbon-lang.github.io) programming language. The full
specification is presented below in human-readable format, and is also exported
as a Zig module for code generation purposes
(see [src/Templater.zig](src/Templater.zig#L79) for example usage)


## Contents

+ [ISA](#isa)
    - [High Level Properties](#high-level-properties)
    - [Parameter Legend](#parameter-legend)
    - [Op Codes](#op-codes)
        <!--#ISA toc-->


## ISA

> [!Caution]
> The ISA is still potentially subject to rapid (breaking) changes

### High level properties

+ 64-bit instructions
+ Little-endian encoding
+ Instruction suffixes for extra large immediates and variable-length operand sets
+ 64-bit registers + stack allocation
+ Separated address spaces for global data, executable, and working memory
+ Heap access controlled by host environment
+ 16-bit indexed spaces for:
    - global data
    - functions
    - blocks within functions
    - effect handler sets
    - effect handlers
+ Floating point values are IEEE754
+ Floats are fixed width, in sizes `32` and `64`
+ Integers are always two's complement
+ Integers are fixed width, in sizes `8`, `16`, `32`, and `64`
+ Sign of integers is not a property of types; only instructions
+ Structured control flow
+ Expression-oriented
+ Effects-aware
+ Tail recursion

### Parameter Legend

| Symbol | Type | Description | Bit Size |
| ------ | ---- | ----------- | -------- |
| `R` | RegisterIndex | Designates a register | `8` |
| `H` | HandlerSetIndex | Designates an effect handler set | `16` |
| `E` | EvidenceIndex | Designates a specific effect handler on the stack of effect handlers | `16` |
| `G` | GlobalIndex | Designates a global variable | `16` |
| `U` | UpvalueIndex | Designates a register in the enclosing scope of an effect handler | `8` |
| `F` | FunctionIndex | Designates a specific function | `16` |
| `B` | BlockIndex | Designates a specific block; may be either relative to the function (called absolute below) or relative to the block the instruction is in, depending on instruction type | `16` |
| `b` | Immediate | Immediate value encoded within the instruction | `8` |
| `s` | Immediate | Immediate value encoded within the instruction | `16` |
| `i` | Immediate | Immediate value encoded within the instruction | `32` |
| `w` | Immediate | Immediate value encoded after the instruction | `64` |


### Op codes

<!--#ISA body-->
