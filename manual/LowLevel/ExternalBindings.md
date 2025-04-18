# External bindings

## Introduction

### Running example

Suppose we have set of C headers that provide functionality for working with
vectors; perhaps these are different headers from the same library, or perhaps
these are from different libraries altogether. Just to have a concrete running
example, let's suppose that `vector.h` provides

```c
typedef struct {
    double x;
    double y;
} vector;

vector* new_vector(double x, double y);
```

which is then used by `vector_rotate.h`

```c
#include "vector.h"

vector* vector_rotate(vector* v, double th);
```

and `vector_length.h`

```c
#include "vector.h"

typedef double len;

len vector_length(vector* p);
```

### Compositionality

Since `vector_rotate.h` includes `vector.h`, when `hs-bindgen` processes the
header it can create a Haskell definition for `Vector` alongside the import of
`vector_rotate`. The problem is that if we then process `vector_length.h`, we
will create a _second_ definition of `Vector`, identical to but incompatible
with the first; the result will be that `vector_rotate` and `vector_length`
cannot be used together.

There are two solutions to this problem. The first is to create a new header
file that includes both (or all three) headers, which we can then process in one
go. This "all or nothing" approach may not be ideal however, especially when
this involves multiple C libraries which may be only tangentially related. It
would be much nicer if we can process libraries separately, and reuse
definitions from one library in another. That is the purpose of external
bindings.

## Generating and using external bindings

When we generate bindings for `vector.h`, we can ask `hs-bindgen` to produce
external bindings in addition to the Haskell module (command line flag
`--gen-external-bindings`). This will result in a file that looks like this:

```yaml
types:
- headers: vector.h
  cname: vector
  package: hs-vector
  module: Vector
  identifier: Vector
```

This says that the C type called `vector`, defined in `vector.h`, should be
mapped to the type called `Vector` defined in module `Vector` from the
`hs-vector` package (rather than _generating_ a definition for it).

We can then use these external bindings when processing `vector_rotate.h`
(command line flag `--external-bindings`). This will result in something like
this:

```haskell
import qualified Vector

foreign import capi safe "vector_rotate.h vector_rotate"
  vector_rotate :: Ptr Vector.Vector -> CDouble -> IO (Ptr Vector.Vector)
```

## Substituting hand-written types

External bindings can also be used to use hand-written types instead of
generated ones. For example, by default `hs-bindgen` would generate the
following definition for `len` in `vector_length.h`:

```haskell
newtype Len = Len {
    un_Len :: CDouble
  }
```

Let's suppose we want to use this hand-written type instead:

```haskell
module Vector.Types where

newtype Length = UnsafeWrap { unwrap :: Double }

instance Show Length where ..

pattern Length :: Double -> Length
pattern Length x <- (unwrap -> x)
  where
    Length x
      | x < 0     = error "Length must be non-negative"
      | otherwise = UnsafeWrap x
```

We can do this by handwriting an external bindings file:

```yaml
types:
- headers: vector_length.h
  cname: len
  package: hs-vector
  module: Vector.Types
  identifier: Length
```

If we then use `--external-bindings` _twice_ when processing `vector_length.h`
(once for the external bindings for `vector.h` and once for the external bindings
for `Length`), we get

```haskell
import qualified Vector
import qualified Vector.Types

foreign import capi safe "vector_length.h vector_length"
  vector_length :: Ptr Vector.Vector -> IO Vector.Types.Length
```

Of course it will be the user's responsibility in this case to ensure that the
Haskell type is compatible with the type that the C code expects.

## Internal headers

Some C libraries make use of internal headers: headers that are not part of
the public API. For example, suppose we have some game library consisting of
two public headers `game_world.h` and `game_player.h`, both of which include
`game_internal.h`:

```c
#include "game_internal.h"

void move_world(game_state v);
```

and

```c
#include "game_internal.h"

void move_player(game_state v);
```

When we write (or generate) external bindings for this library, these external
bindings should ideally not refer to `game_internal.h`: after all, this header
is not part of the public API, and if the library undergoes some internal
refactoring that renames that internal header this should not affect our
external bindings.

For this reason, external bindings can mention more than one header for a
given C name:

```yaml
types:
- headers:
  - game_world.h
  - game_player.h
  cname: game_state
  package: hs-game
  module: Game.State
  identifier: Game_state
```

The C name `game_state` is then considered to be defined "in" `game_world.h` or
`game_player.h`, or in any header file that is (possibly transitively) included
by those two headers.

> [!NOTE]
> Generation of external bindings does not currently have any support for
> declaring headers to be internal, so bindings like the above currently need to
> be handwritten (or adjusted from generated ones).
> https://github.com/well-typed/hs-bindgen/issues/592 .




