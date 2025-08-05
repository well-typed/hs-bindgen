# `hs-bindgen` manual

* [Design goals](DesignGoals.md)
* [Installation](Installation.md)

## Generating low-level bindings

* [Introduction](LowLevel/Introduction.md)

### Usage

* [Invocation](LowLevel/Invocation.md)
* [Tracing](LowLevel/Tracing.md)
* [Clang options](LowLevel/ClangOptions.md)
* [Includes](LowLevel/Includes.md)
* [Parsing, selecing and program slicing](LowLevel/ParsingSelectingAndProgramSlicing.md)
* [Binding specifications](LowLevel/BindingSpecifications.md)
* [Test generation](LowLevel/TestGeneration.md)
* [CrossCompilation](LowLevel/CrossCompilation.md)

### Translation

* [Generated names](LowLevel/GeneratedNames.md)
* [Structs](LowLevel/Structs.md)
* [Enums](LowLevel/Enums.md)
* [Unions](LowLevel/Unions.md)
* [Functions](LowLevel/Functions.md)
* [Global variables and constants](LowLevel/Globals.md)
* [Macros](LowLevel/Macros.md)

## Handwriting high-level bindings

This section of the manual is not yet written. It should include at least a
chapter with examples on how to write high-level bindings, and chapter on memory
management.

In addition, we will have combinators that will make writing the high-level
bindings easier. This is currently under active development.

## Generating high-level bindings

This is not yet supported; see the [roadmap](Roadmap.md) for details.
