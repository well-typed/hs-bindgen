# One module per header

`hs-bindgen-cli preprocess --library DIR` generates one Haskell module for each
header of a C library, in a single invocation.  Every type is defined once, by
the module of its header, and the modules that use the type import it.

Without `--library`, `preprocess` generates one module per invocation.  A
library is then split by hand: one invocation per header, in an order that you
choose, each reading the binding specifications of the ones before it.  See
[Generating multiple modules][manual:multiple-modules].

## Example

The vector library of the [binding specifications
tutorial][manual:binding-specifications-tutorial] has three headers.
`vector_rotate.h` and `vector_length.h` include `vector.h` and use its `vector`
type.

```c
// vector.h
typedef struct {
    double x;
    double y;
} vector;

vector* new_vector(double x, double y);
```

```c
// vector_rotate.h
#include "vector.h"

vector* vector_rotate(vector* v, double th);
```

The following command generates a module for each of the three headers.

```
hs-bindgen-cli preprocess \
    -I c \
    --library c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vectorlibrary \
    --hs-output-dir hs/hs-vector/generated \
    --module VectorLibrary \
    --single-file \
    --unsafe '' \
    vector_rotate.h \
    vector_length.h
```

`vector.h` is not an argument.  It gets a module because the given headers
include it and it is in the library directory `c`.  With `--dry-run`, the
command writes nothing and prints each module with its header:

```
VectorLibrary.Vector: vector.h
VectorLibrary.Vector_rotate: vector_rotate.h
VectorLibrary.Vector_length: vector_length.h
```

Module `VectorLibrary.Vector` defines the Haskell type `Vector`.  Module
`VectorLibrary.Vector_rotate` imports it.  Without its comments, the binding
of `vector_rotate` is:

```haskell
import qualified VectorLibrary.Vector

vector_rotate ::
     BG.Ptr VectorLibrary.Vector.Vector
  -> BG.CDouble
  -> IO (BG.Ptr VectorLibrary.Vector.Vector)
```

## Which headers get a module

A *library header* is a header that

* is one of the given headers, or is included by them, directly or indirectly,
  and
* is in a `--library` directory, or below one.

`--library` can be given more than once.  When one library directory is inside
another, the outer one counts.

A library header gets a module if something is generated for it.  A header
that has only an include guard gets none.  Neither does a header in the
library directory that the given headers do not include: it is not read.  To
leave out a header that is read, deselect its declarations: see [Leaving a
header out](#leaving-a-header-out).

The module is named after the base module (`--module`) and the path of the
header in its library directory.  The extension is dropped.  Each component of
the path becomes a component of the module name: the first letter is
upper-cased, and a character that a module name cannot have becomes `_`.  A
component that starts with a digit or an underscore gets a `C` in front.

| Header in the library directory | Module, with `--module Lib` |
|---------------------------------|-----------------------------|
| `types.h`                       | `Lib.Types`                 |
| `mylib/types.h`                 | `Lib.Mylib.Types`           |
| `foo-bar.h`                     | `Lib.Foo_bar`               |
| `foo.inc.h`                     | `Lib.Foo_inc`               |
| `3d.h`                          | `Lib.C3D`                   |

## Which module a declaration goes to

A header *owns* the declarations that are located in it, and its module has
their bindings.  There are two additions.

* A typedef that is merged with its struct into one Haskell type, as in
  `typedef struct foo foo;`, goes where the struct is, also when the two are
  in different headers.  When the struct is not in a library header, both go
  where the typedef is.
* Nobody owns a declaration from a header outside the library, or one that
  the selection flags do not select.  If a module needs such a type, the first
  module that needs it generates it, and the modules after that one import it
  from there.

## The order of the modules

A module is generated after the modules that it uses.  A header uses another
header when one of its declarations needs a declaration that the other owns,
directly or through other declarations.  The modules are generated in rounds:
each round takes the modules whose uses have all been generated, in the order
in which their headers are first included.

Which header includes which does not decide the order.  In the following
library `b.h` includes `a.h`, and the module of `b.h` comes first, because
`b.h` defines the struct that the function of `a.h` needs.

```c
// a.h
struct shape;

void shape_draw(struct shape *s);
```

```c
// b.h
#include "a.h"

struct shape {
  int sides;
};
```

Haskell modules cannot import each other, so headers that use each other share
one module.  It is named after the header that includes most of the others, or
after the first of them that is included when there is a tie.  `--dry-run`
prints it with all of its headers:

```
Lib.Td: td.h, types.h
```

`--list-base-module-names` prints the names of the modules alone, one per
line, in the order in which the modules are generated.

## Selecting declarations

The selection flags mean what they mean without `--library`.  They select
declarations, by the header that a declaration is in or by the declaration
itself, and a library header owns the declarations in it that are selected.
One thing differs.  When no `--select-...` flag is given, every declaration of
a library header is selected; without `--library` it is the declarations of
the given headers.

A flag that selects by header therefore chooses among the library headers.

* `--select-from-main-headers` selects the declarations of the given headers
  only.  Each of them gets a module, and what they need of the headers that
  they include is generated with them.
* `--select-by-header-path` and `--select-except-by-header-path` match the
  whole path of a header, as they do without `--library`.  That is not the
  path in the library directory, which a module is named after.
* `--select-from-all-headers` and `--select-all` select what is selected
  without them.  A declaration outside the library directories is owned by
  nobody, whatever selects it.  The exception is a struct that a typedef of
  the library is merged with.

The name that `--select-by-decl-name` and `--select-except-by-decl-name` match
has the tag of a struct, union or enum, as in `struct foo`.  When no
declaration of a library header is selected, nothing is written and the run
warns that no declarations matched, as it does without `--library`.

Every module is generated with [program
slicing][manual:program-slicing].  A declaration that is not selected is
still generated when a selected declaration needs it.
`--enable-program-slicing` is accepted and changes nothing.

### Leaving a header out

A library is given as a directory, and a directory tends to have headers that
are not part of what you want to bind: private headers, or the headers of a
dependency.  To leave such a header out, deselect its declarations with
`--select-except-by-header-path`.  The header then owns nothing and gets no
module, with one exception that the next section describes.  It is still
read, and a declaration of it that another header needs is generated by the
first module that needs it.

For a library that is generated from `mylib.h` with `--module MyLib`,
`--dry-run` prints:

```
MyLib.Mylib.Types: mylib/types.h
MyLib.Mylib.Internal: mylib/internal.h
MyLib.Mylib.Ops: mylib/ops.h
MyLib.Mylib: mylib.h
```

With `--select-except-by-header-path 'mylib/internal\.h$'` it prints:

```
MyLib.Mylib.Types: mylib/types.h
MyLib.Mylib.Ops: mylib/ops.h
MyLib.Mylib: mylib.h
```

The pattern is matched against the whole path of the header file.  That is
not the path that `--dry-run` prints, which is the path in the library
directory.  Two things follow.

* A pattern that is anchored at the library directory matches nothing.
  `'^mylib/internal\.h$'` deselects no header, and no message says so.
* A pattern can match a directory above the library.  With the library
  checked out below a directory called `vendor`, the pattern `'vendor/'`
  deselects every declaration, and the run warns that no declarations matched
  and writes nothing.

So end the pattern with `$`, and start it at a directory of the library.  On
Windows the path is written with backslashes: see [Selection
predicates][manual:selection-predicates].

`--select-by-header-path` does the opposite and keeps the headers that it
matches.  Neither flag changes the name of a module.  The library directory
stays the root of the names: with `--library inc`, the header
`inc/b/types.h` is the module `Lib.B.Types` whatever is selected, and with
`--library inc/b` it is `Lib.Types`.

### A header that is left out and still gets a module

A typedef that is merged with its struct is one type with it.  The type is
selected when either of the two is selected, and it belongs to the header of
the struct.  So when the typedef is in another header, deselecting the header
of the struct leaves the typedef selected, the type is still generated, and
the header of the struct keeps its module.

```c
// types.h
typedef struct point point;
```

```c
// point.h
#include "types.h"

struct point { int x; int y; };
```

```c
// shape.h
#include "point.h"

struct shape { point centre; };
```

With `--select-except-by-header-path 'point\.h$'`, `--dry-run` prints what it
prints without the flag, the module of `point.h` included:

```
Lib.Point: point.h
Lib.Shape: shape.h
```

To leave `point.h` out, deselect the typedef as well, by its name:

```
--select-except-by-header-path 'point\.h$' \
--select-except-by-decl-name '^point$'
```

```
Lib.Shape: shape.h
```

Nothing selects the type now, so nobody owns it.  `struct shape` needs it,
and `Lib.Shape` generates it.

The same rule reaches outside the library.  When a typedef of the library is
merged with a struct from a header outside the library, selecting that struct
selects the type, and the type goes to the module of the typedef.

When only one of the two is selected, the module that generates the type can
also depend on how the type is used: see [Limitations](#limitations).

## Binding specifications

An external binding specification (`--external-binding-spec`) is used for
every module.  A type that it binds is not generated and nobody owns it, so
the headers that use the type do not wait for the header of the type.

A prescriptive binding specification (`--prescriptive-binding-spec`) without
`hsmodule` is used for every module.  One with `hsmodule` is used for that
module only.  A type that it omits is left out of that module, and a later
module that needs the type generates it.

A run takes one prescriptive binding specification, because
`--prescriptive-binding-spec` can be given only once.  With `hsmodule`, that
is the configuration of a single module.  To configure types of several
modules, leave `hsmodule` out.  Every module after the one that defines a type
then reports an entry that renames the type or changes its representation as
unused: see Limitations.

The name in `hsmodule` has to be the name of a module of the library, as
`--list-base-module-names` prints it.  The name is not checked, so getting it
right is up to you.  A specification with any other name is used for no
module, and there is no message.

The base module is not a module of the library, so this holds for its name
too.  A specification that names the base module is read when the modules are
planned, and by no module.  What it omits or makes opaque is then neither
omitted nor opaque in any module, and it can still move a type to another
module than that of its header.

`--gen-binding-spec-dir DIR` writes the binding specification of each module
to `DIR`, in a file named after the module, as in `Lib.Types.yaml`.  These are
the specifications that the modules are given for the modules before them.
`--gen-binding-spec` names a single file and is a usage error with
`--library`.

## Options

| Option                       | Effect                                                      |
|------------------------------|-------------------------------------------------------------|
| `--library DIR`              | Generate a module for each header in `DIR`.  Repeatable.    |
| `--dry-run`                  | Write nothing, and print each module with its headers.      |
| `--list-base-module-names`   | Print the names of the modules in the order of generation.  |
| `--gen-binding-spec-dir DIR` | Write a binding specification for each module to `DIR`.     |

`--dry-run` also works without `--library`.  The given headers are one module
then, and it is printed with the headers that own a declaration that it has a
binding for, each written the way that it is included.  The other options are
usage errors without `--library`.

A module is written as up to five files: `Lib/Foo.hs` for the types, and
`Lib/Foo/Safe.hs`, `Lib/Foo/Unsafe.hs`, `Lib/Foo/FunPtr.hs` and
`Lib/Foo/Global.hs` for the rest.  With `--single-file` it is one file.

## When nothing is written

No file is written before every module has been generated and the output has
been checked.  The following end the run with exit code 4.

* None of the headers that were read is in a library directory.
* Two headers get one module name, as `foo-bar.h` and `foo_bar.h` do.  Names
  that differ only in case count as one name, so that a library is the same
  on Linux, macOS and Windows.  [Leave out](#leaving-a-header-out) all but one
  of the headers.  Two headers with the same path in different library
  directories, as `inc/a/types.h` and `inc/b/types.h` with `--library inc/a
  --library inc/b`, also get different names when a directory that has both
  is the library: `--library inc` gives `Lib.A.Types` and `Lib.B.Types`.
* Two modules have a file in common.  The functions of `foo.h` and the types
  of `foo/safe.h` both go to `Lib/Foo/Safe.hs`.  [Leave
  out](#leaving-a-header-out) one of the headers, or use `--single-file`.

A library directory that does not exist is a usage error, with exit code 2.

## Generating one module on its own

A module of a library is what `preprocess` generates without `--library` when
it is given

* the name of the module with `--module`,
* the same headers and Clang options,
* a selection of the declarations that the header owns, with
  `--enable-program-slicing`, and
* the binding specification of every module before it with
  `--external-binding-spec`.

This is the way to look into one module.  For a library that is generated from
`mylib.h` with `--module MyLib`, the module of `mylib/ops.h` is:

```
hs-bindgen-cli preprocess \
    -I include \
    --unique-id org.example.mylib \
    --hs-output-dir out \
    --module MyLib.Mylib.Ops \
    --select-by-header-path 'mylib/ops\.h$' \
    --enable-program-slicing \
    --external-binding-spec specs/MyLib.Mylib.Types.yaml \
    --external-binding-spec specs/MyLib.Mylib.Internal.yaml \
    mylib.h
```

`--select-by-header-path` selects what the header owns, unless a typedef and
its struct are in different headers.  Select by name then.

## Limitations

The headers are read once, as one translation unit, and every module is
generated from it.  Headers that cannot be included together cannot be
generated together.

* A value is generated by every module that needs it.  Binding specifications
  describe types only, so a macro that uses a macro of another header gets a
  definition of that macro in its own module.
* A type that `--hash-define` defines is in no header, so a binding
  specification cannot name it.  Every module that needs it generates it.
* A declaration that is read and then gets no binding still counts as a use.
  A function that takes a pointer to a struct that cannot be translated is
  left out with a warning, and the headers of its other argument types still
  come before its own.
* A declaration that several headers repeat belongs to the first of them,
  unless a later one is given on the command line.
* Documentation comments are taken from the given headers only.  To keep the
  comments of an included header, give that header as an argument too.
* Haskell names are checked for the library as a whole.  Two declarations
  whose names clash are both left out with a warning, also when they would be
  in different modules.
* A function that takes a callback gets no `ToFunPtr` and `FromFunPtr`
  instances for the type of the callback when all the types that the callback
  mentions are in other modules.
* A header behind a symbolic link is a library header only if the file that
  the link points to is in a library directory.
* When a typedef is merged with a struct and the selection flags select only
  one of the two, the type is still generated: see [A header that is left out
  and still gets a module](#a-header-that-is-left-out-and-still-gets-a-module).
  The module that generates it depends on how the type is used.  The selected
  one of the two is owned as usual.  A declaration that uses the type by the
  other name does not wait for the owner, and its module generates the type
  if it comes first.
* An enum with a fixed underlying type that is a typedef of another header
  ends the run with an internal error (`Type not found`) when the module of
  the typedef comes after the module of the enum.  The same happens without
  `--library` when `--enable-program-slicing` brings in the enum and the
  typedef is not selected.

Some messages are less helpful than they are without `--library`.

* A message about the translation unit as a whole is printed once for every
  module, under the name of that module, and once under the name of the base
  module, for the run that decides on the modules.
* An entry of a prescriptive binding specification that gives a type another
  name or representation is reported as unused by every module after the one
  that defines the type.  These are warnings, and errors with
  `--log-as-error-warnings`.
* An entry that is filed under a header that includes the header of its type
  takes effect and is still reported as unused, by every module.  The entries
  of a generated binding specification are filed that way.

The binding specifications that `--gen-binding-spec-dir` writes file their
entries under the headers that were given on the command line.  A later run of
`preprocess` that reads such a file finds the entries only if it includes one
of those headers.

<!-- sources and references -->

[manual:binding-specifications-tutorial]: binding-specifications.md#tutorial
[manual:multiple-modules]: binding-specifications.md#generating-multiple-modules
[manual:program-slicing]: selecting-and-program-slicing.md#program-slicing
[manual:selection-predicates]: selecting-and-program-slicing.md#selection-predicates
