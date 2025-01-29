# `hs-bindgen`: automatically create Haskell bindings from C header files

> [!WARNING]
> This project is under active development, and has not yet been released.
> That said, you might already get some usable results out of it; if something
> breaks, please check the issue tracker to see if the problem is already known,
> and open an issue if not.

## Project goals

### Low-level API

The goal of the low-level API is to generate Haskell bindings from C headers,
in "fire and forget" mode.

The most important existing tools for the generation of Haskell bindings from C
headers, `hsc2hs` and `c2hs`, require a lot of user input (see [Alternative
generators](https://github.com/well-typed/hs-bindgen/tree/main/alternatives) for
a full review): they assist in writing bindings by filling in details about the
C code _when requested_, but the process is still driven by the programmer. The
goal of `hs-bindgen`, inspired by the Rust
[`bindgen`](https://github.com/rust-lang/rust-bindgen) tool, is to have the
entire process be driven by the C header(s) themselves.

The bindings generated in this mode are too low-level for convenient usage in
Haskell applications; for example, `char*` in C translates to `Ptr CChar` in
Haskell, instead of `String` (or `Text`, `ByteString`, ..). Tools like `c2hs`
can be used to define higher-level bindings, but not automatically, and at the
cost of having to learn a rather arcane bespoke syntax. Using `hs-bindgen`, even
if we do not use it to generate high-level Haskell bindings, users can write
their own high-level bindings _on top of_ the low-level bindings, just as
regular Haskell code. This should also improve integration with tooling such as
[HLS](https://github.com/haskell/haskell-language-server). Writing such bindings
could potentially assisted by regular Haskell functions to [capture common
patterns](https://github.com/well-typed/hs-bindgen/issues/27).

### High-level API

The goal of the high-level API is to be able to generate higher-level bindings
using heuristics to recognize particular patterns, with a good set of defaults
that can be extended by users for specific applications.

> [!NOTE]
> We have not yet begun work on the high level API.

## Manual

You can find the (currently still very incomplete) manual in
[manual/](manual/README.md).

