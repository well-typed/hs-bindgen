# Library mode

`hs-bindgen-cli preprocess --library DIR` binds a C library in one run and
gives every header under `DIR` its own Haskell modules. This page explains how
it does that, and why the obvious approaches fall short. It is written for
developers who know `hs-bindgen` but have not read the library mode code. The
[library mode manual page](../manual/low-level/usage/library-mode.md) describes
the feature for users.

The examples build up one small library, `geometry`, step by step. Each step is
a golden test of the `walkthrough` fixture. The headers are in
`hs-bindgen/test-artefacts/headers/library/walkthrough/`, and the output of
each step is in `hs-bindgen/test-artefacts/fixtures/library/walkthrough/`. The
commands on this page run from the `hs-bindgen` package directory.

## What a run produces

The library starts with three headers. `point.h` declares a struct, a global
variable and a function. `shape.h` includes `point.h` and builds on it, and
`io/svg-export.h` includes `shape.h`. Without include guards and comments, they
read:

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

#define SHAPE_MAX_POINTS 64

struct shape {
  struct point *points;
  int count;
};

typedef int (*shape_filter)(const struct shape *shape, void *user);

struct shape *shape_new(int count);
int shape_count_matching(struct shape **shapes, int count, shape_filter filter, void *user);

void shape_for_each_point(struct shape *shape, void (*visit)(struct point *point, void *user), void *user);

// io/svg-export.h
#include "../shape.h"

int svg_export(const struct shape *shape, const char *path);
```

A dry run with these three input headers lists the files that library mode
writes for them:

```
hs-bindgen-cli preprocess \
    -I test-artefacts/headers/library/walkthrough \
    --library test-artefacts/headers/library/walkthrough/geometry \
    --module Geometry \
    --unique-id org.example.geometry \
    --hs-output-dir generated \
    --gen-binding-spec-dir bindingspecs \
    --select-from-library-headers \
    --create-output-dirs \
    --dry-run \
    geometry/point.h \
    geometry/shape.h \
    geometry/io/svg-export.h
```

```
bindingspecs/Geometry.Point.bindingspec.yaml
bindingspecs/Geometry.Shape.bindingspec.yaml
generated/Geometry/Callback.hs
generated/Geometry/Io/Svg_export/FunPtr.hs
generated/Geometry/Io/Svg_export/Safe.hs
generated/Geometry/Io/Svg_export/Unsafe.hs
generated/Geometry/Point.hs
generated/Geometry/Point/FunPtr.hs
generated/Geometry/Point/Global.hs
generated/Geometry/Point/Safe.hs
generated/Geometry/Point/Unsafe.hs
generated/Geometry/Shape.hs
generated/Geometry/Shape/FunPtr.hs
generated/Geometry/Shape/Safe.hs
generated/Geometry/Shape/Unsafe.hs
```

The modules mirror the layout of the headers. Library mode turns each header
into a *unit*: everything the run generates for that header.

A unit's module name is the `--module` prefix followed by the header's path
below the library directory, so `io/svg-export.h` becomes
`Geometry.Io.Svg_export`. In each part of the path, dots are dropped and other
characters that a module name cannot contain become `_`. Leading characters
that are not letters or digits are dropped, a leading digit gets a `C` in
front, and the first letter is upper-cased. The test `layout/module_names` has
an example of each rule.

These rules are not injective and can give two headers the same module name.
If the library also had a header `_shape.h` next to `shape.h`, and since leading
underscores are removed, it becomes `Geometry.Shape` too. On such a collision,
library mode stops before it writes anything (absolute paths shortened):

```
Module name collision: .../geometry/_shape.h and .../geometry/shape.h both get the module name Geometry.Shape
  .../geometry/_shape.h writes Geometry.Shape.bindingspec.yaml, Geometry/Shape.hs
  .../geometry/shape.h writes Geometry.Shape.bindingspec.yaml, Geometry/Shape.hs, Geometry/Shape/FunPtr.hs, Geometry/Shape/Safe.hs, Geometry/Shape/Unsafe.hs
```

The check only counts headers that write at least one file, so if a selection
predicate leaves `_shape.h` empty, the run goes through.

Library mode rejects two units with the same module name even when their files
do not overlap. Otherwise a header with only types and another with only
functions could write `Geometry/Shape.hs` and `Geometry/Shape/Safe.hs`, which
look like the bindings of a single header. A second check catches units that
write the same file under different module names.

The listing above also has one module that belongs to no header,
`Geometry.Callback`. It exists because of `shape_for_each_point`, whose
parameter is a function pointer type. Haskell code passes a callback for it
with `toFunPtr`, which needs a `ToFunPtr` instance for `Ptr Point -> Ptr Void
-> IO ()`. Any header of the library could spell the same function type, and
if each header's modules defined the instance, two modules would end up
defining it twice, leading to duplicate instances. So the instances of every
such type that mentions a library type live in one module, `<base>.Callback`.
No generated module imports `Geometry.Callback`, so user code that uses its
instances needs to import it.

A typedef callback such as `shape_filter` has no such problem. Its types,
`Shape_filter_Aux` and `Shape_filter`, belong to `shape.h`, and so do its
instances, in `Geometry.Shape`.

`--gen-binding-spec-dir` gives every unit with types a binding specification,
here for `Geometry.Point` and `Geometry.Shape`.

We didn't inspect the generated code inside each module, but each module
imports exactly the modules of the headers that its header includes. It would
be fair to assume that the inner workings of Library mode would just walk the
topological order of include dependency graph and process one header at a
time. The next section shows why that is not enough.

## Forward declarations

Now add `canvas.h`. It uses `struct shape`, but declares it forward instead of
including `shape.h`:

```c
struct shape;

struct canvas {
  int width;
  int height;
  struct shape *selection;
};

void canvas_draw(struct canvas *canvas, const struct shape *shape);
```

This is valid C. The types module of `canvas.h` still needs the type from
`shape.h`, but the include graph has no edge for that import, so a plan that
follows include dependencies leaves it out.

The relation that really captures dependencies is the usage declaration graph.
The include graph and the usage graph can disagree in both directions. A
header can include another header without using anything from it, and then its
module imports nothing from the other. And as `canvas.h` shows, a module can
need another module whose header it never includes.

Placing declarations by usage needs every declaration of the library at once,
so library mode handles the whole library in one run. The frontend runs up to
the resolution of binding specifications. Placement then assigns each
declaration to the unit of its header and derives the unit's module name.
Declarations of a header outside the library directories, and declarations
without a header, are *vendor declarations*, and they all go to one more unit
(see [Vendor declarations](#vendor-declarations)). Name mangling, type
translation and selection follow, and then library mode decides every file of
the run. After that, the backend runs once per unit.

## Import cycles

Next come `layer.h` and `scene.h`. `scene.h` includes `layer.h`, which only
declares `struct scene` forward:

```c
// layer.h
#include "shape.h"

#define LAYER_MAX_SHAPES 32

struct scene;

struct layer {
  struct scene *scene;
  struct shape *shapes;
  int count;
};

typedef void (*layer_paint)(struct layer *layer, void *user);

void layer_paint_all(struct layer *layer, layer_paint paint, void *user);

// scene.h
#include "layer.h"

#define SCENE_MAX_SHAPES (LAYER_MAX_SHAPES * 8)

struct scene {
  struct layer *layers;
  int count;
};

struct scene *scene_new(void);
```

The includes form no cycle, but the declarations do, so `Geometry.Layer` and
`Geometry.Scene` need each other.

Library mode breaks the cycle with a [boot
file](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#how-to-compile-mutually-recursive-modules).
A boot file `M.hs-boot` declares part of the module `M`: which types,
instances and values exist. Another module imports it with `import {-#
SOURCE #-} M`, and GHC compiles the boot file before the modules that use it.

Each types module in an import cycle gets a boot file, such as
`Geometry/Layer.hs-boot`:

```haskell
lAYER_MAX_SHAPES :: BG.CInt

data Layer

instance Eq Layer

instance BG.Generic Layer
```

GHC rejects pattern synonyms and type family instances in a boot file. The
missing type family instances include the `FFIType` equations, and since the
boot files can't include them, a types module seen through its boot file
cannot reduce `FFIType` at the argument and result types of a callback. To fix
that a `<M>.Callback` file is created with all the callback related code.

## Vendor declarations

A library usually uses types it does not declare. Single-header mode generates
whatever the selection keeps, wherever it is declared, in its one module.
Library mode does the same, but it puts the vendor declarations in one module,
`<base>.Vendor`. Without program slicing, and with the default selection
predicate, the selection keeps none of them, and a library declaration that
needs a vendor declaration is dropped with a `[select]` warning. With program
slicing, the selection keeps the vendor declarations that the library uses. A
selection predicate that matches a vendor header adds its declarations, with
or without slicing.

The vendor declarations form a unit like the others, so what this page says
about units applies to it too.

## Known limitations and future work

- **Flexible array members in an import cycle.** A struct whose flexible array
  member has the type of a cycle peer, such as `struct A { int n; struct B
  *items[]; };`, becomes a type synonym that names the peer. The boot file
  copies the synonym and imports the peer normally, not through its boot
  file, so GHC reports "Module graph contains a cycle".
- **Several `--library` directories.** When directories overlap, a header
  belongs to the first `--library` that contains it, so module names depend on
  the order of the flags. With `--library inc --library inc/geo`,
  `inc/geo/point.h` is `Lib.Geo.Point`, and with the order reversed it is
  `Lib.Point`.
- **The prescriptive binding specification** applies only when its `hsmodule`
  is the `--module` value, not the module of a header.
- **The dry run does not catch every filesystem error.** For example, a
  read-only output directory passes the dry run and then fails the real run.
