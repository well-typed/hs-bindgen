# `hs-bindgen` manual

* [Design goals](design-goals.md)
* [Installation](installation.md)
* [Terminology](terminology.md)

## Generating low-level bindings

* [Introduction](low-level/introduction.md)

### Usage

* [Invocation](low-level/usage/invocation.md)
* [Tracing](low-level/usage/tracing.md)
* [Clang options](low-level/usage/clang-options.md)
* [Includes](low-level/usage/includes.md)
* [Selecting and program slicing](low-level/usage/selecting-and-program-slicing.md)
* [Binding specifications](low-level/usage/binding-specifications.md)
* [Non-portability](low-level/usage/non-portability.md)
* [Test generation](low-level/usage/test-generation.md)
* [Cross-compilation](low-level/usage/cross-compilation.md)

### Translation

* [Generated names](low-level/translation/generated-names.md)
* [Structs](low-level/translation/structs.md)
* [Enums](low-level/translation/enums.md)
* [Unions](low-level/translation/unions.md)
* [Pointer manipulation API](low-level/translation/pointer-manipulation.md)
* [Functions](low-level/translation/functions.md)
* [Global variables and constants](low-level/translation/globals.md)
* [Macros](low-level/translation/macros.md)

### Appendix

* [Visibility](low-level/appendix/visibility.md)
* [The `Prim` class](low-level/appendix/prim.md)

## Handwriting high-level bindings

This section of the manual is not yet written. It should include at least a
chapter with examples on how to write high-level bindings, and a chapter on
memory management.

In addition, we will have combinators that will make writing the high-level
bindings easier. This is currently under active development.

## Generating high-level bindings

This is not yet supported; see the [roadmap](roadmap.md) for details.
