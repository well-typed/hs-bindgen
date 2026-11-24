# Library mode

A C library usually spreads its API over many headers. Library mode is the
recommended way to generate bindings for such a library as several Haskell
modules. You pass the library's directory with `--library DIR`. A single run
then generates bindings for every header under `DIR` that the input headers
include, directly or through other headers. Each of these headers gets its own
set of modules, named after its path.

You could try to get a similar layout with one `hs-bindgen` run per header,
each passing its binding specifications to the next (see [Generating multiple
modules][manual:binding-specifications-multiple-modules]). We discourage that
approach. Each run sees only part of the library, so the bindings can be
incorrect or even misleading. For example, a header that only forward-declares
a type gets its own empty copy of that type. Headers whose types refer to each
other cannot each get a module of their own. Library mode sees the whole
library at once, so it can decide which header each type belongs to and which
modules import which. It also handles cycles correctly.

## Generating bindings for a library

Take a library with three headers in the directory `geometry`. `point.h`
declares points, `shape.h` builds shapes out of them, and `io/svg-export.h`
writes shapes to files. Here is an excerpt of each:

```c
// point.h
struct point {
  double x;
  double y;
};
extern const struct point point_origin;
double point_distance(const struct point *a, const struct point *b);

// shape.h
#include "point.h"
struct shape {
  struct point *points;
  int count;
};
struct shape *shape_new(int count);
void shape_for_each_point(struct shape *shape, void (*visit)(struct point *point, void *user), void *user);

// io/svg-export.h
#include "../shape.h"
int svg_export(const struct shape *shape, const char *path);
```

The input header `follows_includes.h` is next to the `geometry` directory and
includes all three headers. Run this command from the `hs-bindgen` directory of
the repository:

```
hs-bindgen-cli preprocess \
    -I test-artefacts/headers/library/walkthrough \
    --library test-artefacts/headers/library/walkthrough/geometry \
    --module Geometry \
    --unique-id org.example.geometry \
    --hs-output-dir generated \
    --gen-binding-spec-dir bindingspecs \
    --create-output-dirs \
    follows_includes.h
```

Each header gets the usual modules, named after its path below the `--library`
directory. A header that declares no types, such as `io/svg-export.h`, gets no
types module:

| Header | Modules |
|---|---|
| `point.h` | `Geometry.Point`, `Geometry.Point.Safe`, `.Unsafe`, `.FunPtr`, `.Global` |
| `shape.h` | `Geometry.Shape`, `Geometry.Shape.Safe`, `.Unsafe`, `.FunPtr` |
| `io/svg-export.h` | `Geometry.Io.Svg_export.Safe`, `.Unsafe`, `.FunPtr` |

Characters that cannot appear in a module name become `_`, as in `Svg_export`.
The [module name tests][source:headers/library/layout/module_names] show every
rule. The run also writes `Geometry.Callback` (see [Callback
modules][t:callback-modules]) and one binding specification per header with
types. The full output of this example is in its [golden
files][source:fixtures/library/walkthrough/follows_includes].

`follows_includes.h` itself gets no modules, because it is not under a
`--library` directory.

## Headers whose types refer to each other

Now add `layer.h` and `scene.h`. A layer points at its scene, and a scene points
at its layers:

```c
// layer.h
struct scene;
struct layer {
  struct scene *scene;
  struct shape *shapes;
  int count;
};

typedef void (*layer_paint)(struct layer *layer, void *user);

// scene.h
struct scene {
  struct layer *layers;
  int count;
};
```

Run the command again with `--overwrite-files` and the input header
`import_cycle.h`, which includes all the headers so far and `canvas.h`. That
header uses `struct shape` without including `shape.h`, and `Geometry.Canvas`
still imports `Geometry.Shape`. The run prints a notice:

```
[Notice ] [HsBindgen] [library] Some types modules form an import cycle. Generating boot files for them automatically
```

`hs-bindgen` writes [boot files][ghc:guide:mutually-recursive-modules] for
`Geometry.Layer` and `Geometry.Scene`, and the two modules import each other
through these boot files. `Geometry.Layer`, for example, has this import:

```haskell
import {-# SOURCE #-} qualified Geometry.Scene
```

Compile the bindings with cabal or `ghc --make`, which both handle boot files.
Compiling them one module at a time with `ghc -c` is not supported.

## Callback modules
[t:callback-modules]: #callback-modules

Some `ToFunPtr` and `FromFunPtr` instances (see [Wrapper and dynamic
imports][manual:functions-wrapper-dynamic]) go into callback modules, which no
generated module imports:

* `Geometry.Callback` is shared by the whole library. It holds the instances
  for function pointer types written out in place that mention a library type,
  such as the parameter `visit` of `shape_for_each_point`.
* `Geometry.Layer.Callback` holds the instances for the callback typedefs of a
  header with a boot file, such as `layer_paint` of `layer.h`. Callback
  typedefs of other headers keep their instances in the types module of their
  header.

Import a callback module yourself wherever you need its instances:

```haskell
import Geometry.Callback ()
import Geometry.Layer.Callback ()
```

## Types from headers outside the library
[t:types-outside-the-library]: #types-from-headers-outside-the-library

A library often uses types that it does not declare. Their headers are outside
the `--library` directories.

By default, the selection keeps only the declarations of the library headers.
With `--enable-program-slicing`, the selection also keeps the types that the
library uses, and library mode writes them into one module. Its name is the
`--module` name followed by `.Vendor`. The run prints a notice, and `-v 3`
lists the declarations:

```
[Notice ] [HsBindgen] [library] Library module VendorDependencies.Vendor holds 2 declarations from outside the --library directories
```

With `--gen-binding-spec-dir`, the vendor module gets a binding specification
too. It names each header relative to an `-I` directory where it can, and
otherwise by the input headers that include it, as for the C library headers.
Another run only reuses such an entry if it includes one of those headers.

## Checking first with `--dry-run`

In library mode, `--dry-run` prints the path of every file the run would write,
one per line. For the first command, the list starts with:

```
bindingspecs/Geometry.Point.bindingspec.yaml
bindingspecs/Geometry.Shape.bindingspec.yaml
generated/Geometry/Callback.hs
```

## Name collisions

Two headers can end up with the same module name. Suppose `geometry` also has a
private header `_shape.h` that declares a struct of its own. Module names drop
the leading underscore, so `_shape.h` also gets the module name
`Geometry.Shape`. If the input header is `module_name_collision.h`, which
includes both headers, the run stops before it writes anything (absolute paths
shortened):

```
Module name collision: .../geometry/_shape.h and .../geometry/shape.h both get the module name Geometry.Shape
  .../geometry/_shape.h writes Geometry.Shape.bindingspec.yaml, Geometry/Shape.hs
  .../geometry/shape.h writes Geometry.Shape.bindingspec.yaml, Geometry/Shape.hs, Geometry/Shape/FunPtr.hs, Geometry/Shape/Safe.hs, Geometry/Shape/Unsafe.hs
```

Only headers that write files count toward a collision, so leaving one of the
two headers with nothing to generate fixes the error. Add
`--select-except-by-header-path '/_shape\.h$'` to the command, and keep
`--overwrite-files` as before. Two headers that would write the same file cause
an output path collision, which is the same kind of error.

The [vendor module][t:types-outside-the-library] takes part too. A header such
as `vendor.h` directly under the `--library` directory gets the same module
name as the vendor module, so the run stops if both write files.

## Options

* `--library DIR`: generate modules for the headers under `DIR`. Repeatable.
* `--module NAME`: the base module name of every generated module.
* `--gen-binding-spec-dir DIR`: write a binding specification for each header
  with types into `DIR`, as JSON if `DIR` ends in `.json`. Use it instead of
  `--gen-binding-spec`.
* `--prescriptive-binding-spec FILE`: takes effect only when the `hsmodule` of
  `FILE` is the `--module` name.
* `--enable-program-slicing`: also generate the types from headers outside the
  `--library` directories that the library uses, in one vendor module (see
  [Types from headers outside the library][t:types-outside-the-library]).
* `--generate-hs-boot`: not recommended with `--library`, because it gives
  every types module a boot file, not only those in an import cycle.

[Selection predicates][manual:selecting-and-program-slicing] treat the headers
under the `--library` directories as the main headers. `--library` is not
available with `--single-file`.

The [developer documentation][dev:library-mode] explains how library mode
works and describes its limitations in detail. The [test
fixtures][source:fixtures/library] show the full output for many more cases.

<!-- sources and references -->

[dev:library-mode]: ../../../dev/library-mode.md
[ghc:guide:mutually-recursive-modules]: https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#how-to-compile-mutually-recursive-modules
[manual:binding-specifications-multiple-modules]: binding-specifications.md#generating-multiple-modules
[manual:functions-wrapper-dynamic]: ../translation/functions.md#wrapper-and-dynamic-imports
[manual:selecting-and-program-slicing]: selecting-and-program-slicing.md
[source:fixtures/library]: ../../../hs-bindgen/test-artefacts/fixtures/library
[source:fixtures/library/walkthrough/follows_includes]: ../../../hs-bindgen/test-artefacts/fixtures/library/walkthrough/follows_includes
[source:headers/library/layout/module_names]: ../../../hs-bindgen/test-artefacts/headers/library/layout/module_names
