# Includes

Includes are a surprisingly common source of issues.  Understanding the details
can save time and frustration.

## Terminology

The following terminology is used by `hs-bindgen`.

Header
: A header is a C source file that is generally used to declare data types and
functions, as well as define macros.  Header files use the `.h` extension.
`hs-bindgen` generates bindings from header files.

Include
: An include is the inclusion of a header using a `#include` directive, which is
processed by the C preprocessor.  The recursively processed content of the
referenced header is inserted into the source.  Note that there may be
whitespace between the `#` and `include`, used to visually format blocks within
macro logic.

System include
: A system include is the inclusion of a header using an include directive of
the form `#include <stdio.h>`.  `hs-bindgen` only uses system includes.

Quote include
: A quote include is the inclusion of a header using an include directive of the
form `#include "stdio.h"`.  While `hs-bindgen` does not use quote includes, the
headers being translated may use them, so it is still important to understand
them.

Header resolution
: The preprocessor must locate the source of a header specified in an include
directive, which we call header resolution.  Header resolution is
implementation-specific: it may be done differently in different toolchains.
See below for details about how Clang does header resolution.

C include search path
: A C include search path is a list of directories to search when resolving a
header.  Similar to how the more well-known `PATH` environment variable is a
list of directory paths used to search for executables, `C_INCLUDE_PATH` is a
list of directory paths used to search for headers.  The multiple meanings of
the word "path" is an unfortunate source of confusion, so we take care to
specify *search path* to refer to a list of directories.

Default include directories
: Default include directories are the directories that are in a C include search
path by default.  They generally include *standard system directories* and
*compiler builtin directories*.

## Clang

Clang maintains two separate C include search paths:

* The *system C include search path* initially contains some default include
  directories.
* The *quote C include search path* is initially empty.

Clang modifies these C include search paths based on command-line options and
environment variables, described below.  In general, precedence for each type of
C include search path is as follows:

1. Directories specified via command-line options
2. Directories specified via environment variables
3. Default include directories

### Clang command-line options

This section describes *common* Clang command-line options for configuring C
include search paths.  See the `hs-bindgen` section below for information about
how to use these options via `hs-bindgen`.

The following are used to add a directory to a C include search path:

* `-I <directory>` adds a directory to the system C include search path, with
  precedence over the following options.
* `-isystem <directory>` adds a directory to the system C include search path.
* `-iquote <directory>` adds a directory to the quote C include search path.

These options can be passed multiple times, and the directories are added to
the C include search paths in the same order.

The following are used to disable default include directories:

* `-nostdinc` disables the default include directories (both the standard system
  directories and the compiler builtin directories).
* `-nostdlibinc` disables the standard system directories.
* `-nobuiltininc` disables the compiler builtin directories.

Reference:

* [Include path management](https://clang.llvm.org/docs/ClangCommandLineReference.html#include-path-management)

### Clang environment variables

Environment variable `C_INCLUDE_PATH` is a delimited list of directory paths to
be added to the system C include search path.  Environment variable `CPATH` does
the same.

Linux example:

```
C_INCLUDE_PATH=/some/dep/include:/other/dep/include
```

Windows example:

```
C_INCLUDE_PATH=C:\some\dep\include;C:\other\dep\include
```

These environment variables are used by other compilers as well, including GCC.
If you export these environment variables, then every execution of `clang` and
`gcc` in the environment will also use of them, so care must be taken.  For
example, putting a Clang compiler builtin directory in `C_INCLUDE_PATH` could
result in errors when running `gcc` in the same shell.

Note that there are also system-specific environment variables that may add to
the system C include search path.

### Clang header resolution

Header resolution for a system include is done by searching for the specified
header relative to the following directories:

1. Directories in the system C include search path, in order
2. Directories in the quote C include search path, in order

Header resolution for a quote include is done by searching for the specified
header relative to the following directories:

1. Directory containing the header currently being processed
2. Directories in the quote C include search path, in order
3. Directories in the system C include search path, in order
4. Current working directory

Quote includes are generally used for including headers within a C project.
Since the directory containing the header currently being processed is checked
first, one rarely has to configure a quote C include search path.  System
includes are generally used for including headers of dependencies, including
standard headers.

## `hs-bindgen`

`hs-bindgen` uses `libclang` to resolve headers when generating bindings.  Note
that this is separate from compilation of the generated bindings, described in
the CAPI section below.

`libclang` constructs C include search paths like Clang, but compiler builtin
directories are *not* included by default.  Unless using an alternate standard
library, the compiler builtin directory must be configured using an
command-line option or environment variable.  The compiler builtin directory
should be for the version of `libclang` being used.

`hs-bindgen` passes Clang command-line options to `libclang`.  See
[Clang options][] for details.  Include options that `hs-bindgen` passes to
`libclang` are created based on the `hs-bindgen` command-line options and
environment variables, described below.  In addition, `libclang` process the
Clang environment variables, described above.

[Clang options]: <ClangOptions.md>

### `hs-bindgen` command-line options

`hs-bindgen-cli` provides a `-I <DIRECTORY>` option that adds a directory to the
system C include search path.  It is translated to a Clang `-I` option.

`hs-bindgen-cli` also provides a `--no-stdinc` option that disables the default
include directories.  It is translated to a Clang `-nostdinc` option.

Clang has many more include options, which may be passed via `--clang-option`
options.  For example, a Clang option can be used to configure the quote C
include search path.  Since all `hs-bindgen` includes are system includes, there
is no need to configure the quote include search path for `hs-bindgen`, but
configuration could be necessary for the C project that bindings are being
generated for.

### `hs-bindgen` environment variables

As described in [Clang options][], environment variable
`BINDGEN_EXTRA_CLANG_ARGS` may be used to specify arbitrary Clang options.
Precedence is as follows:

1. `hs-bindgen` command-line options
2. `BINDGEN_EXTRA_CLANG_ARGS`
3. Clang environment variables (such as `C_INCLUDE_PATH`)

### `hs-bindgen` header resolution

Headers to translate are specified as arguments to `hs-bindgen-cli preprocess`
or Template Haskell `hashInclude` function calls.  In addition, headers are
specified in [Binding specifications][].  All of these headers are resolved
via `libclang` using system includes.

[Binding specifications]: <BindingSpecifications.md>

Headers must be specified as they would be in an include directive.  They are
generally relative to a directory in a C include search path, but note that they
are C syntax.  Forward slashes must be used as directory separators, even when
using Windows.  (Backslashes are interpreted as characters in a directory or
file name.)

For example, many Haskell projects store project C code in an `include` and/or
`cbits` directory.  That directory should be added to the system C include
search path, and header arguments should be relative to it.  For example, the
following command runs the preprocessor on header `foo.h` in the `include`
directory:

```
$ hs-bindgen-cli preprocess -I include foo.h
```

This translates to include directive `#include <foo.h>`.  Header resolution
searches for `foo.h` in the system C include search path and finds it in the
`include` directory.

Note that `hs-bindgen-cli preprocess include/foo.h` does not work in general,
even though it is convenient to type with tab-completion.  It translates to
include directive `#include <include/foo.h>`.  Header resolution searches for
`include/foo.h` in the system C include search path, and it will not find
`foo.h` unless you add the current working directory to the system C include
search path.

A C include search path is used to search for C headers, so only adding
directories that contain C header files is best practice.  Adding a directory
that contains C source as well as C header files is a not optimal.  Adding a
directory that contains many types of files (such as a Haskell project
directory) is dirty.  Command `hs-bindgen-cli preprocess -I . include/foo.h`
is therefore a dirty hack, but it works as long as `foo.h` (and any
transitively included headers) do not rely on `include` being in the system C
include search path.

### CAPI

When needed, `hs-bindgen` generates C code that is included in the generated
Haskell module.  When the generated module is compiled, GHC calls a C compiler
to compile the generated C code.  The compiler that GHC is configured to use is
called, which generally defaults to GCC on Linux and Clang on Windows.  Check
the "C compiler command" in the output of `ghc --info` to confirm which compiler
is used.

GHC passes `-I` options to the C compiler to configure the system C include
path.  These options are determined by configuration in the `.cabal` files for
the package and all transitive dependencies, the `cabal.project` and
`cabal.project.local` files for the project, and any command-line configuration
(passed via `cabal` for example).  The order of the `-I` options passed to the C
compiler is determined by GHC.  Environment variables supported by the C
compiler (such as `C_INCLUDE_PATH`) are also relevant.

Note that `hs-bindgen` configuration is not relevant.  Header resolution at this
stage is *not* affected by `hs-bindgen-cli` options or
`BINDGEN_EXTRA_CLANG_ARGS` environment variables.

## Debugging

When you encounter an include issue, the first thing to do is understand where
the issue is occurring.  Possibilities include:

* `hs-bindgen` translation of C headers
    * `hs-bindgen` options are relevant, including `BINDGEN_EXTRA_CLANG_ARGS`.
    * Clang options are relevant, including `C_INCLUDE_PATH`.
    * Run `hs-bindgen-cli --version` to confirm which version of `libclang` is
      being used.
* Compilation of C libraries to shared objects
    * `hs-bindgen` options are not relevant.
    * The compiler options are relevant.  `C_INCLUDE_PATH` is used by both GCC
      and Clang.
* Compilation of translated Haskell modules
    * `hs-bindgen` options are not relevant.
    * The compiler options are relevant.  `C_INCLUDE_PATH` is used by both GCC
      and Clang.
    * Run `ghc --info` to confirm which compiler is being used.

When using the Template Haskell API, you can use the `-ddump-splices` GHC option
to dump the generated Haskell source, including CAPI C source.  This allows you
to confirm which include directives are generated.

The following command runs the Clang preprocessor and displays information that
includes the C include search paths used by Clang.  It takes command-line
options and environment variables into account, so you can test configuration
and confirm precedence.

```
$ clang -E -v - </dev/null
```

Note that the appropriate Clang builtin directory is configured by default, but
it is not configured by default when using `libclang`/`hs-bindgen`.

Similarly, the following command runs the GCC preprocessor and displays
information that includes the C include search paths used by GCC.  It takes
command-line options and environment variables into account, so you can test
configuration and confirm precedence.

```
$ gcc -xc -E -v - </dev/null
```

The `hs-bindgen-cli resolve` command may be used debug `hs-bindgen` header
resolution.

### Common issues

* __`stddef.h` not found__ - This error usually indicates that a compiler
  builtin directory is not configured.  It usually happens when translating
  headers, since `libclang` does not configure a compiler builtin directory by
  default.  You should configure the Clang builtin directory that matches the
  version of `libclang` being used via `BINDGEN_EXTRA_CLANG_ARGS` or an
  `hs-bindgen-cli` `-I` option.
