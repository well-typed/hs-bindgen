# Bundled C example

This example demonstrates using `hs-bindgen` with C source files compiled
directly into the Haskell package via `c-sources`, instead of linking against a
pre-built shared library.

## How it works

The `.cabal` file uses `c-sources` and `include-dirs` instead of
`extra-libraries`:

```cabal
executable bundled-c-bin
  c-sources:    cbits/rect.c
  include-dirs: cbits
```

Cabal compiles `rect.c` and links it into the executable automatically.

**Note:** GHC compiles `c-sources` with its configured C compiler (typically GCC
on Linux), while `hs-bindgen` uses `libclang` to parse headers and derive type
layouts. For simple types this is unlikely to cause problems, but GCC and Clang
can disagree on memory layout for more exotic constructs (bitfields, packed
structs, platform-specific alignment). See the [Compiler choice (GCC
vs. Clang)](../../../manual/LowLevel/Usage/01-Invocation.md#compiler-choice-gcc-vs-clang)
section in the manual for details.

## Prerequisites

No additional prerequisites beyond the standard `hs-bindgen` setup (GHC, Cabal,
LLVM/Clang). No external C libraries need to be installed.

## Running

```bash
cd examples/bundled-c
./generate-and-run.sh
```

The script runs `hs-bindgen-cli` on the header file to generate Haskell
bindings, then builds and runs the executable.
