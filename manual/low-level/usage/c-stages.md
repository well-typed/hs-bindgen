# C stages

Generation of bindings and building the generated bindings may involve two
different C toolchains, running at different times. This documents seeks to
separate these stages clearly, which is important to understand the different
`hs-bindgen` flags.

## The stages

* __Generation stage__: `libclang` parses the headers you specify, and
  `hs-bindgen` translates the declarations it finds into Haskell.

* __Compilation stage__: the generated Haskell modules embed C source (i.e., the
  userland CAPI wrappers). When GHC compiles a generated module, it hands that C
  source to the C compiler GHC is configured to use, which generally defaults to
  GCC on Linux and Clang on Windows. This is a different compiler, in a
  different process, from the one used in the generation stage.

The stages must agree on what the headers declare. When they do not, the
bindings describe one view of the header and the object code implements another;
see [Non-portability][manual:non-portability].

## Root directives

`hs-bindgen` does not parse the headers you specify one at a time. It
synthesises a *root header* and parses that:

```c
#define SUPPORTS_UNICODE 1
#include <generated_names.h>
```

The lines of that header are the *root directives*. At the moment, we support
`#include` directives for the headers you name, and `#define` directives. They
are a single ordered list.

The same rendering is prepended to the C wrapper source of every generated
module. A `#define` stated as a root directive therefore holds in *both* C
stages, from one declaration site.

| Root directive     | Command line               | Template Haskell          |
|--------------------|----------------------------|---------------------------|
| `#include <a.h>`   | `a.h` (positional)         | `hashInclude "a.h"`       |
| `#define FOO 1`    | `--hash-define FOO 1`      | `hashDefine "FOO" "1"`    |
| `#define FOO`      | `--hash-define FOO ''`     | `hashDefine "FOO" ""`     |
| `#define FOO(x) x` | `--hash-define 'FOO(x)' x` | `hashDefine "FOO(x)" "x"` |

### Order matters

A `#define` only affects the headers that follow it. The command line

```console
hs-bindgen-cli preprocess --hash-define A 1 a.h --hash-define B 1 b.h ...
```

produces the root header

```c
#define A 1
#include <a.h>
#define B 1
#include <b.h>
```

so `a.h` is parsed with `A` defined and `B` undefined. This is why
`--hash-define` is position-sensitive (unlike the C compiler command line option
`-D`), and why `hashDefine` must precede the `hashInclude` calls it applies to.

### Differences between `--hash-define` syntax and `-D` syntax

`--hash-define` syntax follows `#define` syntax, and translates differently than `-D` arguments.

| C compiler `-D` argument | Command line               | Template Haskell          | Emitted directive  |
|--------------------------|----------------------------|---------------------------|--------------------|
| `-D FOO`                 | `--hash-define FOO 1`      | `hashDefine "FOO" "1"`    | `#define FOO 1`    |
| `-D FOO=BAR`             | `--hash-define FOO BAR`    | `hashDefine "FOO" "BAR"`  | `#define FOO BAR`  |
| `-D FOO=`                | `--hash-define FOO ''`     | `hashDefine "FOO" ""`     | `#define FOO`      |
| `-D 'FOO(x)=x'`          | `--hash-define 'FOO(x)' x` | `hashDefine "FOO(x)" "x"` | `#define FOO(x) x` |

In particular, `-DFOO` and `-DFOO=` define *different* macros: the replacement
list is `1` for the former and empty for the latter. Both are `#ifdef`-true,
which is why the difference is easy to miss, but `#if FOO` is true for the
first and an error for the second.

`hs-bindgen` validates neither the name nor the replacement list. A malformed
definition is reported by Clang as a diagnostic in the root header. Unresolvable
`#include`s have the same behavior.

The command-line option takes *two* arguments, which has two consequences:

* Omitting the value silently consumes the next header argument: `--hash-define
  FOO a.h` defines `FOO` as `a.h` and leaves no header to translate.
* A value starting with `-` is rejected as an unknown option. Write
  `--hash-define MIN '(-1)'`, which is better C anyway since the replacement
  list is substituted literally, or place `--` before it. Everything after `--`
  is positional, so no further `--hash-define` may follow.

### Root-header `#define`s are declarations

A `#define` stated as a root directive defines a macro just like a `#define` in
a header, and `hs-bindgen` treats it as a declaration. The same holds for a
`-D` Clang option. Neither is in a header on the file system, and the default
selection predicate does not select them. If you want to select declarations
defined via `-D` or `--hash-define`, use `--select-all` or
`--select-by-decl-name`, or use program slicing when a selected declaration
uses the macro. For example, given the header

```c
struct S { T x; };
```

and `--hash-define T int`, `hs-bindgen` deselects `S`, because it depends on
`T`. With `--enable-program-slicing`, it generates a newtype `T` for the macro,
and binds the field `x` to it.

A header that redefines such a macro differently conflicts with it, just as two
differing definitions in one header do.

`--select-all` also selects the `-D` options the Clang driver adds by itself,
such as `-D__GCC_HAVE_DWARF2_CFI_ASM=1`, and so does `--select-by-decl-name`
when its pattern matches their names. Use `--select-from-all-headers` to select
every declaration in a header, but none of these macros.

Also generated binding specifications omit `-D` and `--hash-define` macros,
because they are not in any header. The binding specification generated for the
example above thus records `S`, but not `T`. A package that uses this binding
specification, and the macro `T` as well, binds its own newtype `T`, distinct
from the type of the field `x`. Binding specifications for these macros are
tracked in [issue #2284][issue:2284].

### Include directories and other Clang options cannot be root directives

There is no C syntax for "add a directory to the include search path", so `-I`
cannot be a root directive: it configures the generation stage only. The
compilation stage takes its include directories from the `.cabal` file
(`include-dirs`) and the `cabal.project` files (`extra-include-dirs`), and you
must keep those in agreement. See [Includes][manual:includes].

The same holds for Clang options generally. `--clang-option`,
`--clang-option-before`, `--clang-option-after` and `BINDGEN_EXTRA_CLANG_ARGS`
are passed to `libclang` in the generation stage and are *not* forwarded to the
compilation stage. A `-D` passed through one of those channels therefore defines
the macro for the generation stage alone; use a root directive instead. See
[Clang options][manual:clang-options].

## Header-only libraries

Some libraries ship their implementation in the header, behind a macro:

```c
int header_only_horns(void);

#ifdef HEADER_ONLY_IMPLEMENTATION
int header_only_horns(void) { return 1; }
#endif
```

Such a macro must *not* be a root directive. It would be emitted into the
wrapper source of every generated module, and each of those translation units
would define the function, giving duplicate symbols at link time.

Instead, the macro should only be defined when building a C object out of the
library header. If the library was shipped with a built C (shared) object, then
all is good. However, if the C object is intended to be built by you, then it
is up to you to ensure that the macro is defined when compiling the header into
a C object.

One way to ensure that the C object is built with the macro defined is to bundle
the C header with your own Cabal package. Define the macro and include the
header in exactly one C source file and include it in the Cabal package like so:

```c
/* cbits/header_only.c */
#define HEADER_ONLY_IMPLEMENTATION
#include <header_only.h>
```

```cabal
library
  c-sources: cbits/header_only.c
```

and generate the bindings *without* the define:

```console
hs-bindgen-cli preprocess \
  -I c \
  --module HeaderOnly \
  --hs-output-dir generated \
  header_only.h
```

The generated wrappers see the declarations only, and the symbols come from the
`cbits` object at link time. In this manual, we does exactly that:
[`header_only.h`][header:header_only.h],
[`cbits/header_only.c`][source:header_only.c] and
[`Manual/HeaderOnly.hs`][source:Manual/HeaderOnly.hs].

Where the header cannot be included without its implementation, the remaining
option is to make the macro a root directive and generate a *single* module
(`--single-file`), so that there is only one wrapper translation unit.

<!-- sources and references -->

[header:header_only.h]: ../../c/header_only.h
[issue:2284]: https://github.com/well-typed/hs-bindgen/issues/2284
[manual:clang-options]: clang-options.md
[manual:includes]: includes.md
[manual:non-portability]: non-portability.md
[source:Manual/HeaderOnly.hs]: ../../hs/manual/app/Manual/HeaderOnly.hs
[source:header_only.c]: ../../hs/manual/cbits/header_only.c
