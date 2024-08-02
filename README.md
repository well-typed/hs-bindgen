# `hs-bindgen`: automatically create Haskell bindings from C header files

> [!WARNING]
This project is in early stages of development. Do not use.

## Project goals

The most important existing tools for the generation of Haskell bindings from C
headers, `hsc2hs` and `c2hs`, require a lot of user input (see [Alternative
generators](https://github.com/well-typed/hs-bindgen/tree/main/alternatives) for
a full review): they assist in writing bindings by filling in details about the
C code _when requested_, but the process is still driven by the programmer. The
goal of `hs-bindgen`, inspired by the Rust
[`bindgen`](https://github.com/rust-lang/rust-bindgen) tool, is to have the
entire process be driven by the C header(s) themselves.

### Execution mode

It should be possible to run the tool [as a
preprocessor](https://github.com/well-typed/hs-bindgen/issues/9), or in
[Template Haskell mode](https://github.com/well-typed/hs-bindgen/issues/11),
offering a convenient workflow

```haskell
module MyModule

generateBindingsFor "path/to/foo.h"
```

### Cross-compilation

We should support [cross
compilation](https://github.com/well-typed/hs-bindgen/issues/10), ideally in
both execution modes, but definitely in preprocessor mode.

We need to do this reliably, which means that we need to use existing
infrastructure (for example, to find out the offsets of all fields inside a
struct). We will therefore [bind to
`libclang`](https://github.com/well-typed/hs-bindgen/issues/20).

### No (or very limited) bespoke syntax

One of the downsides of working with `c2hs` is that users need to learn a new
(frankly rather arcane) syntax. We want to limit any new syntax that users might
have to learn, working primarily with just regular Haskell. The low-level /
high-level split we propose (see below) is part motivated by this requirement:
even if we do not use the tool to generate high-level Haskell bindings, users
can write their own, by writing regular Haskell code that happens to work with
the (generated) low-level bindings. This should also improve integration with
tooling such as [HLS](https://github.com/haskell/haskell-language-server).

For the high-level bindings this is more challenging, as users will need to be
provided with [ways to customize decisions made by the
tool](https://github.com/well-typed/hs-bindgen/issues/21). A good option for
power-users here might be to offer `hs-bindgen` as a library, so that
customization can be done again with regular Haskell code.

## Roadmap

The project is split up into three major milestones, each of which are useful in their
own right and can be released as version
[0.1](https://github.com/well-typed/hs-bindgen/issues/29),
[0.2](https://github.com/well-typed/hs-bindgen/issues/30) and
[0.3](https://github.com/well-typed/hs-bindgen/issues/31).

### [Milestone 1: `Storable` instances](https://github.com/well-typed/hs-bindgen/milestone/2)

The object here is to be able to generate Haskell types with `Storable`
instances for "all"
[`struct`](https://github.com/well-typed/hs-bindgen/issues/14),
[`enum`](https://github.com/well-typed/hs-bindgen/issues/15) and
[`union`](https://github.com/well-typed/hs-bindgen/issues/16) definitions found
in the C header.

This will require a mechanism to select which instances are of interest, perhaps
[similar to those supported by Rust
bindgen](https://github.com/well-typed/hs-bindgen/issues/12), or through some
kind of ["program slicing"](https://github.com/well-typed/hs-bindgen/issues/13),
starting with a set of functions the user is interested in. This is especially
important because headers can `#include` other headers.

The explicit goal of this milestone and the next one is to generate low-level
bindings that mirror the C definitions exactly. So, for example, if a struct
contains a field of type `char*`, the corresponding field in the Haskell
type will have type `Ptr CChar`. Constructing higher-level bindings
(where we might use `String`, for example), will not be considered until
[Milestone 3: High-level API](https://github.com/well-typed/hs-bindgen/milestone/4).
As such, it should be possible to generate these bindings with minimal user
input or customization, ideally none (apart from selection).

We should also [generate a
test-suite](https://github.com/well-typed/hs-bindgen/issues/22) to check that
the `Storable` instances we generate are correct.

### [Milestone 2: Low-level API](https://github.com/well-typed/hs-bindgen/milestone/3)

The goal of this milestone is to [generate low-level `foreign import`
declarations](https://github.com/well-typed/hs-bindgen/issues/25) for all
functions declared in the header file. Like in milestone 1, the goal here is to
avoid needing user input as much as possible, though some decisions do need to
be made (for example, should calls be `safe` or `unsafe`?).

Whenever possible, if the C header contains documentation, we should also
[include that documentation as
Haddocks](https://github.com/well-typed/hs-bindgen/issues/26) in the generated
bindings.

We should support functions that [accept or return `struct`s by
value](https://github.com/well-typed/hs-bindgen/issues/37), by generating
appropriate wrappers for them.

We should also generate binding for
[constants](https://github.com/well-typed/hs-bindgen/issues/41) and [global
variables](https://github.com/well-typed/hs-bindgen/issues/42).

While some for users these low-level bindings might be useable as-is, the
primary objective here is to make it easier for users to manually write
high-level bindings; this is now regular Haskell coding, and should be well
supported by tooling such as HLS.

We might want to release this together with milestone 2.5, see below.

### [Milestone 2.5: Library support for hand-written high-level bindings](https://github.com/well-typed/hs-bindgen/milestone/6)

This milestone sits in between milestones 2 and 3 because it is useful for both.
When hand-writing high-level bindings, there are undoubtedly a lot of patterns
that emerge. We should [capture these as Haskell functions or type
classes](https://github.com/well-typed/hs-bindgen/issues/27) and [release this
as a separate library
`hs-bindgen-patterns`](https://github.com/well-typed/hs-bindgen/issues/28).

### [Milestone 3: High-level API](https://github.com/well-typed/hs-bindgen/milestone/4)

Even in the ideal case that _all_ patterns that are used in the construction
of the high-level bindings can be expressed using the patterns provided by the
`hs-bindgen-patterns` library from milestone 2.5, it might still be cumbersome
to have to write them all out, and so some generation might still be useful.

This is all the more important for data type declarations (as opposed to
function definitions); we'll want to try and generate high-level equivalents
for [`struct`s](https://github.com/well-typed/hs-bindgen/issues/39),
[`enum`]s(https://github.com/well-typed/hs-bindgen/issues/40), and
[(tagged) unions](https://github.com/well-typed/hs-bindgen/issues/18).

However, there is a trade-off here. There are _lot_ of [decisions that need to
be made for the high-level
bindings](https://github.com/well-typed/hs-bindgen/issues/21): the C header file
does not provide sufficient information by itself. This means that the tool must
be customizable, for example through a DSL, through annotations in the C header
files themselves, or through using `hs-bindgen` as a library with customizations
as regular Haskell code. It is conceivable that in cases that would require
extensive customization, perhaps the most direct way to do that customization is
not to use generation at all, but simply write bindings manually, provided that
the `hs-bindgen-patterns` library provides sufficient support.

Nonetheless, there will probably be scenarios where a [set of defaults and
heuristics](https://github.com/well-typed/hs-bindgen/issues/32) can do a good
job at generating high-level bindings, without much -- or any -- input from
the user.

To make tweaking of the output easier, the tool should include [comments in the
generated code that explain tool
decisions](https://github.com/well-typed/hs-bindgen/issues/23). In other words,
the generated code should provide sufficient information to the user to allow
them to change the way that the code is generated.

### [Milestone 4: Additional features](https://github.com/well-typed/hs-bindgen/milestone/5)

This milestone is currently just a collection of additional features that we
might consider, such as

* generating bindings for [C preprocessor
  macros](https://github.com/well-typed/hs-bindgen/issues/43).
* generating bindings for [function
  addresses](https://github.com/well-typed/hs-bindgen/issues/46)

