# `hs-bindgen` manual

* [Design goals](DesignGoals.md)
* [Installation](Installation.md)

## Generating low-level bindings

* [Introduction](LowLevel/Introduction.md)

### Usage

* [Invocation](LowLevel/Usage/01-Invocation.md)
* [Tracing](LowLevel/Usage/02-Tracing.md)
* [Clang options](LowLevel/Usage/03-ClangOptions.md)
* [Includes](LowLevel/Usage/04-Includes.md)
* [Parsing, selecing and program slicing](LowLevel/Usage/05-ParsingSelectingAndProgramSlicing.md)
* [Binding specifications](LowLevel/Usage/06-BindingSpecifications.md)
* [Test generation](LowLevel/Usage/07-TestGeneration.md)
* [Cross-compilation](LowLevel/Usage/08-CrossCompilation.md)

### Translation

* [Generated names](LowLevel/Translation/01-GeneratedNames.md)
* [Structs](LowLevel/Translation/02-Structs.md)
* [Enums](LowLevel/Translation/03-Enums.md)
* [Unions](LowLevel/Translation/04-Unions.md)
* [Zero-Copy](LowLevel/Translation/05-ZeroCopy.md)
* [Functions](LowLevel/Translation/06-Functions.md)
* [Global variables and constants](LowLevel/Translation/07-Globals.md)
* [Macros](LowLevel/Translation/08-Macros.md)

### Appendix

* [Visibility](LowLevel/Appendix/01-Visibility.md)

## Handwriting high-level bindings

This section of the manual is not yet written. It should include at least a
chapter with examples on how to write high-level bindings, and a chapter on
memory management.

In addition, we will have combinators that will make writing the high-level
bindings easier. This is currently under active development.

## Generating high-level bindings

This is not yet supported; see the [roadmap](Roadmap.md) for details.
