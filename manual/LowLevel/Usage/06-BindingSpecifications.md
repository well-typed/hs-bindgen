# Binding specifications

TODO

## Binding specification files

TODO

## Binding specification generation

TODO

## External bindings

TODO

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

### Generating and using external bindings

When we generate bindings for `vector.h`, we can ask `hs-bindgen` to produce
external bindings in addition to the Haskell module (command line flag
`--gen-binding-spec`). This will result in a file that looks like this:

```yaml
version:
  hs_bindgen: 0.1.0
  binding_specification: '1.0'
hsmodule: Vector
ctypes:
- headers: vector.h
  cname: vector
  hsname: Vector
- headers: vector.h
  cname: struct vector
  hsname: Vector
hstypes:
- hsname: Vector
  instances:
  - Eq
  - Show
  - Storable
```

When used as an external binding specification, this says that the C types
called `vector` and `struct vector`, defined in `vector.h`, should both be
mapped to the type called `Vector` defined in module `Vector`
(rather than _generating_ one or more Haskell types).

We can then use these external bindings when processing `vector_rotate.h`
(command line flag `--external-binding-spec`). This will result in something
like

```haskell
import qualified Vector

$(addCSource "...")
foreign import ccall safe "vector_rotate_interface_function"
  vector_rotate :: Ptr Vector.Vector -> CDouble -> IO (Ptr Vector.Vector)
```

### Substituting hand-written types

External bindings can also be used to use hand-written types instead of
generated ones. For example, by default `hs-bindgen` would generate the
following definition for `len` in `vector_length.h`:

```haskell
newtype Len = Len {
    unwrapLen :: CDouble
  }
```

Let's suppose we want to use this hand-written type instead:

```haskell
module Vector.Types where

newtype Length = UnsafeWrap { unwrap :: Double }

instance Show Length where ...

pattern Length :: Double -> Length
pattern Length x <- (unwrap -> x)
  where
    Length x
      | x < 0     = error "Length must be non-negative"
      | otherwise = UnsafeWrap x
```

We can do this by handwriting an external bindings file:

```yaml
version:
  hs_bindgen: 0.1.0
  binding_specification: '1.0'
hsmodule: Vector.Types
ctypes:
- headers: vector_length.h
  cname: len
  hsname: Length
```

If we then use `--external-binding-spec` _twice_ when processing
`vector_length.h` (once for the external bindings for `vector.h` and once for
the external bindings for `Length`), we get

```haskell
import qualified Vector
import qualified Vector.Types

$(addCSource "...")
foreign import ccall safe "vector_length_interface_function"
  vector_length :: Ptr Vector.Vector -> IO Vector.Types.Length
```

Of course it will be the user's responsibility in this case to ensure that the
Haskell type is compatible with the type that the C code expects.

### Internal headers

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
version:
  hs_bindgen: 0.1.0
  binding_specification: '1.0'
hsmodule: Game.State
ctypes:
- headers:
  - game_player.h
  - game_world.h
  cname: game_state
  hsname: Game_state
```

The C name `game_state` is then considered to be defined "in" `game_world.h` or
`game_player.h`, or in any header file that is (possibly transitively) included
by those two headers.

In this example, module `Game.State` represents the declarations in
`game_internal.h`.  A binding specification that only refers to the public
headers can be generated as follows:

* The public headers should be specified as inputs.  Note that an internal
  header should never be specified as input.
* All declarations in the main header directories should be parsed.  This is the
  default.
* A predicate should be specified so that only the desired declarations in the
  internal header are selected.  Selecting by header path makes it easy to
  select all declarations in the internal header.

## Standard library bindings

TODO

## Prescriptive binding specifications

TODO

> [!NOTE]
> In a binding specification, a `cname` specifies the (qualified) C name for a
> type, after anonymous declarations are named. A prescriptive binding
> specification can specify Haskell names (`hsname`). However, these Haskell
> names do not affect selection, which also matches against the (qualified) C
> names.

## Non-portability

The bindings generated by `hs-bindgen` should be regarded as build artefacts!

`hs-bindgen` relies on `libclang` for all machine-dependent decisions: struct
layout, type sizes, alignment, and the resolution of C preprocessor (CPP)
conditionals. This means that the shape of the generated bindings and the
corresponding binding specifications, depends on the system where `hs-bindgen`
runs. In general, generated bindings are *not* portable across platforms,
architectures, or even different configurations of the same library.

### Explicit conditional compilation

Consider a header that uses `#ifdef` to vary its layout:

```c
typedef struct {
    int type;
#ifdef USE_EXTENDED
    double extra;
#endif
    int value;
} my_record;
```

When `hs-bindgen` processes this header with `-DUSE_EXTENDED`, the generated
struct will have three fields (`type`, `extra`, `value`). Without that flag, it
will have only two (`type`, `value`). The generated binding specification will
differ accordingly.

### Configure-generated headers

A more subtle case arises with libraries whose public headers are generated by
`./configure`, CMake, or similar build systems. These tools probe the host
system and inject `#define`s that silently affect the public API. For example, a
library might generate a `config.h` that is included transitively by the public
headers:

```c
/* generated by ./configure */
#define HAVE_IPV6 1
#define SIZEOF_LONG 8
```

Nothing in the public header itself signals that it is platform-specific, and
`hs-bindgen` has no way to distinguish configure-generated content from
hand-written content. The generated bindings will simply reflect whichever
`#define`s were in effect at the time.

### Dealing with non-portability

There are several strategies for managing non-portable bindings.

#### Pass `-D` flags explicitly

When generating bindings, pass `-D` flags to ensure that the CPP resolution
matches the intended target. There are three mechanisms for doing so (see
[Clang options](03-ClangOptions.md) for details):

* `hs-bindgen-cli preprocess --clang-option="-DUSE_EXTENDED" ...`
* `BINDGEN_EXTRA_CLANG_ARGS="-DUSE_EXTENDED" hs-bindgen-cli preprocess ...`
* `--clang-option-before` / `--clang-option-after` for controlling precedence

#### Keep `-D` flags consistent

If a C header requires certain CPP flags to be set (e.g. `-DUSE_IPV6`), those
same flags must be passed both to `hs-bindgen` when generating bindings *and* as
C compiler options in the `cabal.project` file (via `ghc-options`), or the
`.cabal` file (via `cc-options`), when compiling the generated bindings.
Otherwise, `hs-bindgen` will generate bindings against one view of the header,
but `cabal` will compile against a different view, leading to missing symbols
or type mismatches.

#### Treat binding generation as part of the build process

Do not assume that generated binding specifications are portable across machines.
On each target platform, regenerate the bindings using that platform's headers
and configuration (perhaps using `SetupHooks`). At present `hs-bindgen` does
not yet provide any explicit support for making this process easier.

#### Gate platform-specific packages

If distributing generated bindings (e.g. on Hackage), gate them to the
platform(s) they were generated on. For example, in a `.cabal` file:

```cabal
if !(os(linux) && arch(x86_64))
  buildable: false
```

See [Installation](../../Installation.md) for further discussion of
platform-specific considerations.

The [`bundled-c` example](../../../examples/bundled-c/) provides a concrete
demonstration. Its C header uses `#ifdef RECT_3D` to optionally add a struct
field; The `test-cpp-mismatch.sh` script exercises the most subtle case:
bindings are generated *without* the flag but the C code is compiled *with*
it, causing silent runtime data corruption.
