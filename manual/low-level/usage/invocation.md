# Invocation

`hs-bindgen` provides three methods for generating Haskell bindings from C
header files:

1. Command-line invocation via `hs-bindgen-cli preprocess`
2. Cabal preprocessor integration using literate Haskell files
3. Template Haskell mode via the `HsBindgen.TH` module

> [!NOTE]
> This documentation is for `hs-bindgen` version 0.1.0.

## Command-line invocation

The `preprocess` command generates Haskell bindings from C header files.  It
uses `libclang` to parse headers and produces Haskell modules containing the
bindings.

### Basic usage

```bash
hs-bindgen-cli preprocess [OPTIONS] HEADER_FILE ...
```

On Windows:

```powershell
hs-bindgen-cli.exe preprocess [OPTIONS] HEADER_FILE ...
```

### Options

The `preprocess` command accepts several categories of options.

#### Module generation

Options controlling module generation:

- `--hs-output-dir DIR` - Output directory for generated modules
- `--module NAME` - Base module name (e.g., `Generated.MyLib`)
- `--unique-id ID` - Unique identifier for C wrapper functions (e.g., `org.example.mylib`)
- `--create-output-dirs` - Create output directories if they do not exist

#### Input

The headers to translate are given as positional arguments. They are interleaved
with `#define` root directives:

- `HEADER` - Emit `#include <HEADER>`
- `--hash-define NAME VALUE` - Emit `#define NAME VALUE` before the following
  headers

Root directives are ordered, and apply to the compilation of the generated C
source as well. See [C stages][manual:c-stages].

#### Clang configuration

Options configuring `libclang`:

- `-I DIR` - Add include directory
- `--gnu` - Enable GNU extensions
- `--c-standard STANDARD` - Specify C standard (c89, c99, c11, c17)
- `--clang-option OPT` - Pass arbitrary option to Clang
- `--clang-option-before OPT` - Pass option before managed options
- `--clang-option-after OPT` - Pass option after managed options

See [Clang options][manual:clang-options] for details about the order in which
options are passed to Clang.

#### Selection predicates

Options determining which parsed declarations are included in the generated
bindings:

- `--select-by-header-path PATTERN` - Select declarations from headers matching pattern
- `--select-by-decl-name PATTERN` - Select declarations with C names matching pattern
- `--select-except-by-decl-name PATTERN` - Exclude declarations with C names matching pattern
- `--select-except-deprecated` - Exclude deprecated declarations
- `--enable-program-slicing` - Enable program slicing (includes transitive dependencies)

With program slicing disabled (the default), only declarations matching select
predicates are included.  With program slicing enabled, transitive
dependencies of selected declarations are included, even if explicitly
deselected.

See [Selecting, and program slicing][manual:selecting-and-program-slicing] for
details.

#### Macros

- `--parse-empty-macros` - Parse macros with an empty replacement list

By default, a macro such as `#define FOO` is not parsed, since include guards
have this shape. With `--parse-empty-macros`, such macros are passed to the
macro language, and the ones it declines are reported like any other macro that
failed to translate. See [Macros][manual:macros] for details.

### Example

The following example is adapted from `examples/libpcap/generate.sh`:

```bash
hs-bindgen-cli preprocess \
    -I "./libpcap" \
    --unique-id org.hs-bindgen.libpcap \
    --hs-output-dir hs-project/src \
    --create-output-dirs \
    --module Generated.Pcap \
    --gnu \
    --select-by-header-path pcap.h \
    --enable-program-slicing \
    --select-except-deprecated \
    --select-except-by-decl-name 'pcap_open' \
    pcap.h
```

### Verbosity

The `-v` option controls verbosity:

- `-v1` - Warnings only
- `-v2` - Info messages
- `-v3` - Debug messages
- `-v4` - Trace messages

Higher verbosity levels show which declarations are selected or deselected, and which macros succeed or fail to parse.

### Other commands

Besides `preprocess`, `hs-bindgen-cli` provides:

- `gen-tests` - Generate test cases for bindings
- `binding-spec` - Manage binding specifications
- `info` - Query information (libclang, headers, etc.)

Run `hs-bindgen-cli --help` for details.

### Exit codes

`hs-bindgen` uses the following exit codes:

- 0: Success
- 1: Unexpected errors (panics)
- 2: CLI usage errors
- 3: Invocation of `libclang` has failed
- 4: An `hs-bindgen`-specific error has happened

## Library mode
[t:library-mode]: #library-mode

When `--library DIR` is passed, `preprocess` switches to library mode. It
first runs the frontend over the root header(s), to learn which declarations
get bindings and which of them use which, and assigns each library header its
own Haskell module (headers whose declarations use each other share one, see
[processing order][t:processing-order]). It then runs the binding generator
once per module, after the modules it uses. Each of these steps is a complete
run that parses the root header(s) again, so a run takes longer the more
modules there are. Each step receives the binding specifications from all
previous steps as external binding specifications, so cross-module type
references resolve correctly.

This automates the multi-module workflow described in the [binding
specifications][manual:binding-specifications-multi] section.

### Basic usage

```
hs-bindgen-cli preprocess \
    -I /usr/include \
    --library /usr/include/rpm \
    --hs-output-dir gen \
    --create-output-dirs \
    --overwrite-files \
    --module RPM \
    rpm/rpmlib.h
```

This command:

1. Parses `rpm/rpmlib.h` (resolved via `-I /usr/include`) with every header
   it includes, directly or through other headers, and runs the frontend over
   them with program slicing, selecting the declarations of every header under
   `--library`.
2. Keeps the headers under `--library /usr/include/rpm` in which something is
   generated. `rpmlib.h` does not include every header in that directory, and
   the ones it does not reach get no module (see [module-generation
   scope][t:library-scope]).
3. Orders those headers so that each comes after the headers whose
   declarations it uses. Headers whose declarations use each other form one
   group; most groups hold a single header.
4. For each group, derives a Haskell module name, constructs a selection
   predicate targeting the group's declarations, enables program slicing, and
   runs the binding generator.
5. Chains binding specifications: each step receives the binding specifications
   from all previous steps as external binding specifications, so cross-module
   type references resolve.
6. Prints one line saying what it generated:
   `Generated 10 modules from 13 headers (1 declaration loop) in gen`.

A step that fails stops the run with a non-zero exit code, and the modules
written before it stay in place. The other flags in this section only make
sense with `--library`: passing one without it is a usage error (exit code 2).

### Module-generation scope
[t:library-scope]: #module-generation-scope

`--library DIR` defines which headers get their own Haskell module. A header
in the include graph gets a module if and only if its normalised path falls
under a library directory and something in it is generated. An umbrella header
that only includes other headers, with or without an include guard, gets no
module, and neither does a header whose declarations all fail to parse or are
bound by an external binding specification. `DIR` has to be an existing
directory; anything else is a usage error (exit code 2).

Library mode does not scan `DIR`. The include graph holds the headers named on
the command line and the headers they include, directly or through other
headers. A header in `DIR` that none of them includes gets no module, and
nothing reports it: `--dry-run` only lists the headers that do get one. To
cover more of the library, name more headers:

```
hs-bindgen-cli preprocess \
    --library /usr/include/rpm \
    ... \
    rpm/rpmlib.h rpm/rpmlog.h rpm/rpmurl.h
```

This generates 12 modules where `rpm/rpmlib.h` alone gives 10. All the headers
are parsed together, as if one header included them in that order.
A header named on the command line is treated like any other: when it is not
under a library directory it gets no module, and its declarations are only
generated where a library declaration needs them.

`--except-library PCRE` leaves out headers whose normalised path matches the
pattern, even when they are under a library directory. Types from such
headers remain available to other modules through program slicing and binding
spec chaining: each is generated in the first module that needs it.

The pattern is matched against the absolute path, and it matches when any part
of the path does. A bare word such as `'internal'` leaves out
`rpm/internal.h`, but also every header of a library that was unpacked below
`/home/me/internal-tools/`, and then the run generates nothing. Say where in
the path the match has to be:

```
hs-bindgen-cli preprocess \
    --library /usr/include/rpm \
    --except-library '/internal\.h$' \
    --except-library '/rpm/private/' \
    ...
```

The first pattern leaves out `internal.h` in any directory, the second every
header below a directory `rpm/private`.

Note: `-I` is the clang search path only; it tells clang where to find headers
during parsing and has no effect on which headers get modules.

### Selection predicates in library mode
[t:library-mode-predicates]: #selection-predicates-in-library-mode

`--library` and `--except-library` control which *headers* get modules.
Declaration predicates (`--select-by-decl-name`,
`--select-except-by-decl-name`, `--select-except-deprecated`) control which
*declarations* get bindings within each generated module. Without a positive
predicate, every declaration in a module's headers is selected, rather than
only the main headers as in single-header mode. A declaration the predicate
leaves out is still generated when a selected declaration needs it, by program
slicing, in the first module with a declaration that needs it; later modules
import it from there. If its own header has nothing else, the plan lists a
module for that header that is not written, and the summary line still counts
it.

Header predicates (`--select-from-main-headers`,
`--select-from-main-header-dirs`, `--select-from-all-headers`,
`--select-by-header-path` and `--select-except-by-header-path`) cannot be used
with `--library`, and passing
one is a usage error (exit code 2). Each module already selects the
declarations of its own headers, so a header predicate could only leave modules
from the plan empty. Use `--except-library` to leave a header out.

### Module naming
[t:module-naming]: #module-naming

The module name of a header follows its path below the `--library` directory
(both are normalised first, so symlinks and `..` segments are resolved):

1. Take the path relative to the library directory, without the file
   extension: `rpm/rpmio.h` gives `rpm/rpmio`.
2. Split it at the directories: `rpm` and `rpmio`.
3. Turn each part into a module name component: `Rpm` and `Rpmio`.
4. Join the components with dots, after the base module name:
   `RPM.Rpm.Rpmio`.

| `--library` | Header path | Module name |
|---|---|---|
| `/usr/include/rpm` | `/usr/include/rpm/rpmlib.h` | `RPM.Rpmlib` |
| `/usr/include/rpm` | `/usr/include/rpm/rpmio.h` | `RPM.Rpmio` |
| `/usr/include` | `/usr/include/rpm/rpmio.h` | `RPM.Rpm.Rpmio` |

`--library` can be given several times. When a header is under more than one
of the directories, its name follows the path below the shortest of them,
whatever the order of the flags: with both `--library /usr/include` and
`--library /usr/include/rpm`, `rpmio.h` is `RPM.Rpm.Rpmio`.

Step 3 uses the rules that turn C names into Haskell type names, since a
module name component has to follow the same rules as a type name. A name that
those rules allow only gets its first letter uppercased. For the others:

- a character that is not a letter, a digit, an underscore or a single quote
  becomes an underscore;
- a name that does not start with a letter gets a `C` in front, and its first
  letter is uppercased.

| Directory or file name | Component |
|---|---|
| `rpmio` | `Rpmio` |
| `glib-object` | `Glib_object` |
| `foo.bar` (from `foo.bar.h`) | `Foo_bar` |
| `3d` | `C3D` |

Some headers have to share a module (see [processing
order][t:processing-order]). Such a module is named after its outermost
header: the one that includes, directly or through other headers, the most of
the other headers in the module, which is usually the header a C program
includes to use them. If several headers include equally many, the first by
path wins. In RPM, `argv.h`, `rpmtag.h`, `rpmtd.h` and `rpmtypes.h`
share `RPM.Rpmtd`, because `rpmtd.h` includes the other three. `--dry-run`
shows which module each header ends up in.

### Processing order
[t:processing-order]: #processing-order

Contrary to what one might expect, the right order in which to process the
headers is not the topological order of the include graph. Library mode
follows the declaration usage graph instead, lifted to headers: a header comes
after every header whose declarations its own declarations use, directly or
through declarations in other headers. This ensures that:

* a type lands in the module of the header that defines it, even when another
  header declares it forward and uses it first;
* when a module is generated, every library type it uses already has a binding
  specification from an earlier module, so program slicing does not pull the
  types of another library header into it;
* a planned module has something to hold, so `--dry-run` and
  `--list-base-module-names` show the modules a real run writes;
* headers share a module only when they need each other: each has a
  declaration that uses a declaration of the other, directly or through other
  headers. The plan and the summary call such a group a declaration loop.

The plan is made from the declarations that get bindings, and these points
hold for what it knows. [Limits of the plan][t:plan-limits] lists the cases in
which a step generates something the plan did not expect.

Take a header that declares a struct forward and uses it, and a header that
includes it and defines the struct:

```c
// a.h
struct S;
void f(struct S *s);

// b.h
#include "a.h"
struct S { int x; };
```

Processing in include order puts `a.h` first, because `b.h` includes it. The
step for `a.h` selects `f`, program slicing pulls in `struct S`, and so `S`
ends up in `Lib.A`. The step for `b.h` then finds `S` already generated and
writes nothing, although the plan listed `Lib.B`. In declaration order `f`
uses `S`, so `b.h` comes first: `S` lands in `Lib.B`, and the bindings for `f`
import it from there.

A typedef for a struct that another header defines is a forward declaration
too:

```c
// types.h
typedef struct foo foo;
typedef int status;

// foo.h
#include "types.h"
struct foo { status last; };
```

`hs-bindgen` generates a single Haskell type for the struct and its typedef,
and library mode puts it where the struct is defined: `Foo` lands in
`Lib.Foo`, after `Lib.Types`, whose step leaves the typedef alone. A typedef
for a struct from a header that gets no module is different, because no later
step would generate the struct. There the type lands in the module of the
header with the typedef.

The declaration order also decides how library mode deals with cycles, of
which there are two kinds. Headers that include each other do not need to
share a module:

```c
// a.h
#pragma once
#include "b.h"
typedef int a_t;

// b.h
#pragma once
#include "a.h"
typedef int b_t;
```

No include order puts each of these headers after the other, so a plan based on
the include graph has to generate one module for both. Their declarations do
not use each other, so library mode generates `Lib.A` and `Lib.B`.

Declarations can also use each other across headers whose includes form no
cycle at all. This is a reduced form of RPM's `rpmtypes.h` and `rpmtd.h`:

```c
// types.h
typedef unsigned int tag_t;
typedef struct td_s *td;

// td.h
#include "types.h"
struct td_s { tag_t tag; void *data; };
```

The include graph puts `types.h` first, and its step pulls `struct td_s` into
`Lib.Types` because `td` points to it. Yet no order of the two headers is
right: `td` needs `struct td_s` from `td.h`, which needs `tag_t` from
`types.h`, so one module per header would mean two modules that import each
other. Library mode generates both headers into a single module, `Lib.Td`,
named as described under [module naming][t:module-naming].

Two headers also share a module when no declaration is on a loop, as long as
each header needs the other:

```c
// window.h
struct widget;
struct window { int width; int height; };
void window_add(struct window *w, struct widget *child);

// widget.h
struct window;
struct widget { int id; };
struct window *widget_parent(struct widget *w);
```

Neither struct uses the other, but `window_add` needs `struct widget` and
`widget_parent` needs `struct window`. A step generates the types and the
functions of a header together, and it needs the binding specifications of
every header it uses, so neither header can come first. Both go into
`Lib.Widget`, and the plan reports them as a declaration loop.

A loop that passes through declarations in a header outside the `--library`
directories, or in one left out by `--except-library`, still puts the library
headers on it into one module. The other header gets no module.

Binding specifications you pass take part in this. A type bound by an external
binding specification is not generated, so using it does not count, and
neither do the fields of a struct that a prescriptive binding specification
makes `emptydata`. Either can break a loop: binding `tag_t` externally, or
making `struct td_s` opaque, gives `types.h` and `td.h` a module each again.

### Limits of the plan
[t:plan-limits]: #limits-of-the-plan

Binding specifications describe types only, and the plan only knows the
declarations that get bindings. Three things follow.

Values are generated wherever they are needed. A macro that uses a macro from
another header brings a copy of it along:

```c
// limits.h
#define LIM_MAX 64

// buf.h
#include "limits.h"
#define BUF_SIZE (LIM_MAX * 2)
```

`lIM_MAX` is generated in `Lib.Limits` and again in `Lib.Buf`, next to
`bUF_SIZE`. Constants of an anonymous `enum` are copied in the same way. Code
that imports both modules has to qualify such a name, or hide one of the two.

A declaration that gets no bindings still pulls in what it uses:

```c
// api.h
struct session;
typedef int api_status;
struct api_scale { long double factor; };
api_status api_apply(struct session *s, struct api_scale *by);

// session.h
#include "api.h"
struct session { int id; api_status last; };
```

`long double` is not supported, so neither `struct api_scale` nor `api_apply`
gets bindings, and the plan sees nothing in `api.h` that uses `session.h`. It
puts `api.h` first. The step for `api.h` still follows `api_apply` to `struct
session` and generates it in `Lib.Api`. The step for `session.h` then has
nothing left: it warns that no declarations matched the selection predicate,
`Lib.Session` is not written, and the summary line still counts it.

A selection predicate that leaves declarations out moves types in the same
way, see [selection predicates in library mode][t:library-mode-predicates].

### Dry run and module listing

`--dry-run` prints the processing plan (which headers produce which modules)
and exits without generating any files. Useful for verifying the `--library`
and `--except-library` filters before running the full generation. Headers
whose declarations use each other are listed with the same module name, and
the first line counts the declaration loops and the headers in which nothing
is generated.

`--list-base-module-names` prints the base module names one per line and exits.
Each module is listed once, including those shared by a declaration loop. A base
module whose headers declare no types is not written itself, only those of its
`Safe`, `Unsafe`, `FunPtr` and `Global` submodules that it has something for,
so the list is not a complete `exposed-modules` list.

Both flags run the checks of a real run before they print anything. A module
name collision, a prescriptive binding specification for a module that is not
generated, or a `--gen-binding-spec-dir` directory that does not exist stops
them with the same error and exit code.

### Binding specifications

Library mode writes one binding specification per module and passes each to
the later steps as an external binding specification. By default these files
live in a temporary directory that is removed when the run ends. To keep them,
pass `--gen-binding-spec-dir DIR`:

```
hs-bindgen-cli preprocess \
    --library /usr/include/rpm \
    --gen-binding-spec-dir binding-specs \
    ...
```

Each specification goes to a path derived from its module name, so
`RPM.Rpmio` ends up in `binding-specs/RPM/Rpmio.yaml`. Pass these files
to later `hs-bindgen` runs with `--external-binding-spec` so they reuse the
generated types instead of generating their own.

An entry in these files does not name the header that declares its type. It
names the headers given on the command line that reach it, which is
`rpm/rpmlib.h` for every entry of the run above. A later run finds the entry
only when its own header includes one of them. A run on a header that
includes `rpm/rpmio.h` and not `rpm/rpmlib.h` finds none: it drops the
declarations that use the type or, with program slicing, generates the type a
second time, and says nothing about the specification. If later runs are to
include any header of the library, name every library header on the command
line of the library run.

As with `--hs-output-dir`, `DIR` must exist unless `--create-output-dirs` is
given, and existing files are only replaced with `--overwrite-files`.

`--gen-binding-spec` names a single file, which cannot hold one specification
per module, so passing it together with `--library` is a usage error (exit
code 2).

External binding specifications passed with `--external-binding-spec` apply
to every step. So does a prescriptive binding specification passed with
`--prescriptive-binding-spec`, as long as it leaves out `hsmodule`: each entry
then takes effect in whichever module its type ends up in, which also makes it
the way to rename one of two types whose Haskell names would collide. An entry
that matches no declaration in the library is reported once, before any module
is generated. With an `hsmodule`, the specification only applies to the module
of that name, so it plays no part in the [processing
order][t:processing-order]: that module's step reports the entries that match
nothing, and every other step warns that the specification cannot be used.
An `omit` entry in such a file keeps the type out of that module only, so a
later module that uses the type generates it. To omit a type from the whole
library, leave `hsmodule` out.
The name has to be that of a module library mode generates. A header that
shares a module with others has no module of its own, so a specification
naming it would never apply: the run stops with an error before generating
anything. `--dry-run` without the specification shows the module of each
header.

### Module name collisions

The naming scheme can produce collisions. Library mode detects them before
generating any files and exits with an error. Known cases:

- Two headers that differ only in the capitalisation of the first character
  collide: `foo.h` and `Foo.h` both produce component `Foo`.

- Characters that a module name cannot contain all become underscores, so
  `foo-bar.h`, `foo.bar.h` and `foo_bar.h` all produce component `Foo_bar`.
  Likewise, a name that does not start with a letter gets a `C` in front, so
  `3d.h` and `C3D.h` both produce component `C3D`.

- Each base module `M` can expand to category submodules `M.Safe`, `M.Unsafe`,
  `M.FunPtr`, and `M.Global`. So, for example, `foo.h` (module `M.Foo`) and
  `foo/safe.h` (module `M.Foo.Safe`) can collide at `M/Foo/Safe.hs`. A module
  only writes the files it has something for: types go to the base module, a
  function to `Safe`, `Unsafe` and `FunPtr`, a global variable to `Global`.
  The two headers collide when `foo.h` has a function and `foo/safe.h` has a
  type, its own or one it generates for a header without a module. A `foo.h`
  with only types collides with nothing, and neither does a `foo/safe.h` with
  only functions, which writes `M/Foo/Safe/Safe.hs` and its siblings. With
  `--single-file` every module is one file, and this case does not arise.

To resolve a collision, use `--except-library` to leave out one side, or
adjust the `--library` directories to change the derived relative paths.

For now, library mode has no option to override the module name of an
individual header.

## Cabal preprocessor integration

`hs-bindgen` can integrate with Cabal's build system using the literate
Haskell preprocessor mechanism.  This approach provides seamless integration
without requiring external build systems or custom setup scripts.

### Background

Binding generation requires running `hs-bindgen` before GHC compiles Haskell
code.  Several approaches exist to orchestrate this:

1. **External build system** - Use Make, Nix, or similar tools to run
   `hs-bindgen-cli preprocess` before Cabal
2. **Custom setup script** - Write a `Setup.hs` that invokes `hs-bindgen-cli`
   (discouraged; poor tooling integration, particularly with HLS)
3. **Cabal hooks** - Use Cabal's hooks infrastructure (requires very recent
   Cabal versions; not yet fully explored)
4. **Literate preprocessor** - Configure `.lhs` files to use `hs-bindgen-cli`
   as the preprocessor (this section)

The literate preprocessor approach (option 4) leverages Cabal's support for
literate Haskell.  Haskell modules in Cabal can have the `.lhs` extension to
mark them as literate Haskell.  When compiling such files, Cabal runs them
through a preprocessor (normally `unlit`) to generate the `.hs` file before
compilation.  The preprocessor can be changed using the `-pgmL` GHC flag.

By configuring `hs-bindgen-cli` as the preprocessor, binding generation occurs
automatically during `cabal build`.  Instead of literate Haskell markup, the
`.lhs` file contains configuration flags for `hs-bindgen` in the form of a
Haskell list.

A minimal demonstration of the literate preprocessor mechanism (independent of
`hs-bindgen`) is available [here][example:literate-example].

### Configuration

Add the following to your `.cabal` file:

```cabal
library
  exposed-modules:     MyBindings
  hs-source-dirs:      src
  other-extensions:    ForeignFunctionInterface
  build-tool-depends:  hs-bindgen:hs-bindgen-cli
  ghc-options:         -pgmL hs-bindgen-cli -optL tool-support -optL literate
  build-depends:       base, hs-bindgen-runtime
  default-language:    Haskell2010
```

The GHC options specify:
- `-pgmL hs-bindgen-cli` - Use `hs-bindgen-cli` as the literate Haskell
  preprocessor
- `-optL tool-support -optL literate` - Pass arguments to the literate
  preprocessor

### Literate Haskell file

Create a file `src/MyBindings.lhs` containing a Haskell list of command-line
arguments:

```haskell
[ "-I", "./c-lib"
, "--module=MyBindings"
, "--unique-id", "org.example.mybindings"
, "--gnu"
, "--enable-program-slicing"
, "mylib.h"
]
```

This list contains the same arguments you would pass to `hs-bindgen-cli
preprocess`, in standard Haskell list syntax.

The `.lhs` file can contain arbitrary content; it is simply passed to the
preprocessor.  The preprocessor is responsible for parsing the file and
generating Haskell code.

### Build process

When `cabal build` is invoked:

1. Cabal detects the `.lhs` file

2. Cabal invokes the following command:

    ```
    hs-bindgen-cli tool-support literate src/MyBindings.lhs
    ```

3. `hs-bindgen-cli` reads the configuration in `src/MyBindings.lhs` and
  generates bindings like the following command:

    ```
    hs-bindgen-cli preprocess \
      -I ./c-lib \
      --module=MyBindings \
      --unique-id org.example.mybindings \
      --gnu \
      --enable-program-slicing \
      mylib.h
    ```

4. Cabal writes the generated code to a file under `dist-newstyle`.

5. Cabal invokes GHC to compile that file.

### Example

See `examples/literate-example/` for a complete example using the Cabal
preprocessor integration.

## Template Haskell mode

The `HsBindgen.TH` module provides a Template Haskell interface for generating
bindings inline within Haskell modules.  Bindings are generated at compile
time and become part of the module.

### Setup

Enable Template Haskell and import the module:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import HsBindgen.TH
```

Add `hs-bindgen` to `build-depends` in your `.cabal` file:

```cabal
build-depends: base, hs-bindgen, hs-bindgen-runtime
```

### Basic usage

Use `withHsBindgen` with `hashInclude` to generate bindings:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module MyBindings where

import HsBindgen.TH
import Optics ((&), (%), (.~))

let cfg :: Config
    cfg = def & #clang % #extraIncludeDirs .~ [PkgDir "my-c-lib"]

    cfgTH :: ConfigTH
    cfgTH = def & #verbosity .~ Verbosity Warning
 in withHsBindgen cfg cfgTH $
      hashInclude "mylib.h"
```

The `withHsBindgen` function takes three arguments:

1. `Config` - Configuration for binding generation
2. `ConfigTH` - Template Haskell-specific configuration
3. Template Haskell splice specifying what to bind (typically `hashInclude`)

### Configuration

#### Config

The `Config` type configures binding generation.  Common fields:

- `#clang % #extraIncludeDirs` - Include directories
  - `PkgDir "path"` - Path relative to the package root; an absolute path is an
    error, because the package root would be discarded
  - `AbsDir "/path"` - Absolute path; a relative path is resolved against the
    working directory of the compiler invocation and therefore warned about
- `#clang % #gnu` - GNU extensions (`GnuEnabled` or `GnuDisabled`)
- `#clang % #cStandard` - C standard (e.g., `C99`, `C11`)
- `#selectionPredicate` - Select a subset of declarations
- `#programSlicing`     - Use or do not use program slicing

See the `HsBindgen.TH` module documentation for all configuration options.

#### ConfigTH

The `ConfigTH` type configures Template Haskell behavior:

- `#verbosity` - Log level (`Verbosity Silent`, `Verbosity Warning`,
  `Verbosity Info`, `Verbosity Debug`)
- `#customLogLevelSettings` - Fine-grained logging (e.g.,
  `[EnableMacroWarnings]`)

### Complete example

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module CompleteExample where

import HsBindgen.TH
import Optics ((&), (%), (.~))

let cfg :: Config
    cfg = def
            & #clang % #extraIncludeDirs .~ [
                  PkgDir "my-c-library"
                , AbsDir "/usr/local/include"
                ]
            & #clang % #gnu .~ GnuEnabled
            & #clang % #cStandard .~ C11
            & #select % #predicate .~ SelectByHeaderPath (Regex "mylib\\.h")
            & #select % #programSlicing .~ ProgramSlicingEnabled

    cfgTH :: ConfigTH
    cfgTH = def
              & #verbosity .~ Verbosity Info
              & #customLogLevelSettings .~ [EnableMacroWarnings]
 in withHsBindgen cfg cfgTH $
      hashInclude "mylib.h"
```

### Viewing generated code

Use GHC's `-ddump-splices` option to view generated code:

```bash
cabal build --ghc-options="-ddump-splices"
```

### Multiple headers

Call `hashInclude` multiple times:

```haskell
let cfg = def
 in withHsBindgen cfg def $ do
      hashInclude "header1.h"
      hashInclude "header2.h"
```

### Macro definitions

Use `hashDefine` for macros the headers expect. It is `#define` syntax, not C
compiler `-D` syntax, and it only affects the `hashInclude`s that follow it:

```haskell
let cfg = def
 in withHsBindgen cfg def $ do
      hashDefine "USE_EXTENDED" "1"
      hashInclude "header1.h"
```

The definition also reaches the generated C source that GHC compiles. See [C
stages][manual:c-stages].

### Troubleshooting

**"Not in scope" errors:** Verify that `{-# LANGUAGE TemplateHaskell #-}` is
enabled, `HsBindgen.TH` is imported, and `hs-bindgen` is in `build-depends`.

**"Could not find header" errors:** Check include directories in
`#clang % #extraIncludeDirs`.  Try absolute paths if relative paths fail.

**Long compilation times:** Reduce selected declarations via predicates, or
use command-line or preprocessor invocation instead.  Use higher verbosity
(`-v3`) to see what is being processed.

## Preparation of system environment for `hs-bindgen`

See the [Installation][manual:installation] guide for platform-specific setup
instructions (Linux, macOS, Windows, Nix).

## Using `hs-bindgen` with bundled C source files

All examples in the preceding sections assume that the C library you are
binding to is built separately and linked as a shared library. However, when
you are writing your own C code alongside your Haskell project, you can
compile it directly into the package using Cabal's `c-sources` field.  This
eliminates the need for a separate build step, `extra-libraries`,
`extra-lib-dirs`/`extra-include-dirs` in `cabal.project.local`, and
`LD_LIBRARY_PATH` at runtime.

### `.cabal` configuration

Instead of `extra-libraries`, use `c-sources` and `include-dirs`:

```cabal
executable my-app
  main-is:        Main.hs
  hs-source-dirs: app generated
  c-sources:      cbits/my_lib.c
  include-dirs:   cbits
  build-depends:
    , base
    , hs-bindgen-runtime
```

Cabal compiles the listed C files and links them into the executable
automatically.

### Generating bindings

Run `hs-bindgen-cli` on the header file as usual:

```bash
hs-bindgen-cli preprocess \
    -I cbits \
    --hs-output-dir generated \
    --module MyLib \
    --create-output-dirs \
    --overwrite-files \
    my_lib.h
```

Since the C code is compiled by Cabal, there is no need to update
`cabal.project.local` with library paths or set `LD_LIBRARY_PATH`.

>[!NOTE]
>
> When using `c-sources`, GHC compiles the C files with its configured C
> compiler (typically GCC on Linux), while `hs-bindgen` uses `libclang` to
> parse the headers and derive type layouts.  For simple types this is
> unlikely to cause problems, but GCC and Clang can disagree on memory layout
> for more exotic constructs (bit-fields, packed structs, platform-specific
> alignment).  See [Clang vs. GCC][manual:installation-clang-vs-gcc] for
> details.

A complete working example is available in
[`examples/bundled-c`][example:bundled-c].

## Warnings and errors

When there are warnings/errors, understanding what is displaying them can help
with debugging.  Running `cabal build` with the `-j1` option to turn off
parallel building can make the context easier to understand when `hs-bindgen`
is used to generate code in more than one module.

`hs-bindgen` traces are easy to distinguish because they are formatted like
the following, with the log level, source, and trace ID displayed in brackets.

```
[Warning] [HsBindgen] [select-parse] 'struct foo' at "./ex.h 1:9":
  Could not select declaration:
    Unsupported long double
```

`hs-bindgen` uses `libclang` to parse headers.  All `libclang` warnings/errors
are output in the context of an `hs-bindgen` trace.

```
[Error  ] [Libclang ] [clang] ./ex.h:2:3: error: unknown type name 'intt'
Call to 'libclang' returned an error
```

The generated Haskell source code generally contains C source code, which
includes the specified headers.  When Cabal invokes GHC to compile the
generated code, GHC invokes a C compiler to compile any C source code.  That C
compiler may also output warnings/errors, which are output in the context of a
GHC error.  An easy way to distinguish warnings/errors output by the GHC C
compiler is that the source line is displayed twice: once by the C compiler and
again by GHC.

```
In file included from /tmp/ghc2319546_0/ghc_1.c:1:0: error:

/path/to/ex.h:1:20: error:
     warning: ‘foo’ used but never defined
        1 | static inline void foo(void);
          |                    ^~~
  |
1 | static inline void foo(void);
  |                    ^
```



<!-- sources and references -->

[example:bundled-c]: ../../../examples/bundled-c
[example:literate-example]: ../../../examples/literate-example
[manual:binding-specifications-multi]: binding-specifications.md#generating-multiple-modules
[manual:c-stages]: c-stages.md
[manual:clang-options]: clang-options.md
[manual:installation]: ../../installation.md
[manual:installation-clang-vs-gcc]: ../../installation.md#clang-vs-gcc
[manual:macros]: ../translation/macros.md
[manual:selecting-and-program-slicing]: selecting-and-program-slicing.md
