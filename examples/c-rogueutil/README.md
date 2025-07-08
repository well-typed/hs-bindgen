# RogueUtil Haskell Bindings

This example demonstrates generating Haskell bindings for the
[rogueutil](https://github.com/sakhmatd/rogueutil) library, a cross-platform C
library for terminal manipulation.

## Quirks and Workarounds

### 1. Disabling Shared Libraries (`shared: False`)

The `generate-and-run.sh` script sets `shared: False` in `cabal.project.local` to prevent duplicate symbol errors during compilation.

**Why this is needed:**

RogueUtil is an unusual C library that defines function implementations
directly in the header file (`rogueutil.h`) rather than in separate `.c`
files. This is unorthodox C practice, but allows rogueutil to be a header-only
library.

When `hs-bindgen` generates bindings for such a library, it creates C wrapper
functions that get embedded into multiple generated modules (Safe, Unsafe,
FunPtr, Global). During dynamic library compilation, each module includes its
own copy of these wrappers, causing "multiple definition" linker errors like:

```
ld: error: multiple definition of 'printXY'
```

Setting `shared: False` disables dynamic library building, preventing these
conflicts. Static linking only includes each symbol once, avoiding the
duplication problem.

### 2. Enabling Modern C Standard (`-D_POSIX_C_SOURCE=200809L`)

The binding generation command includes the clang option
`-D_POSIX_C_SOURCE=200809L`:

```bash
--clang-option=-D_POSIX_C_SOURCE=200809L
```

This enables POSIX.1-2008 features that rogueutil depends on (such as
`usleep`, `nanosleep`, and terminal control functions). Without this flag,
clang would fail to parse the header due to missing standard library
declarations.

## Running the Example

```bash
./generate-and-run.sh
```
