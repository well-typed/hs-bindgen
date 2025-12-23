# Includes

Configuring command-line options and environment variables so that C `#include`
directives resolve to the intended headers can be surprisingly tricky.  For
example, include directive `#include <stdint.h>` may need to resolve to file
`/usr/include/stdint.h` on a given system.  Understanding the details can save
time and frustration.

## Terminology

The following terminology is used by `hs-bindgen`.

* __Header__: A header is a C source file that is generally used to declare data
  types and functions, as well as define macros.  Header files use the `.h`
  extension.  `hs-bindgen` generates bindings from header files.

* __Include__: An include is the inclusion of a header using a `#include`
  directive, which is processed by the C preprocessor.  The recursively
  processed content of the referenced header is inserted into the source.  Note
  that there may be whitespace between the `#` and `include`, used to visually
  format blocks within macro logic.

* __Bracket include__: A bracket include is the inclusion of a header using an
  include directive of the form `#include <stdio.h>`.  `hs-bindgen` only uses
  bracket includes.

* __Quote include__: A quote include is the inclusion of a header using an
  include directive of the form `#include "stdio.h"`.  While `hs-bindgen` does
  not use quote includes, the headers being translated may use them, so it is
  still important to understand them.

* __Header resolution__: The preprocessor must locate the source of a header
  specified in an include directive, which we call header resolution.  Header
  resolution is implementation-specific: it may be done differently in different
  toolchains.  See below for details about how Clang does header resolution.

* __C include search path__: A C include search path is a list of directories to
  search when resolving a header.

* __Default include directories__: Default include directories are the
  directories that are in a C include search path by default.  They generally
  include *standard system directories* and *compiler builtin directories*.

* __System header__: A system header is essentially a header that a user cannot
  easily change themselves.  Clang distinguishes system headers so that it can
  suppress warnings that originate with them, which it does by default.

## Clang

This section describes common Clang usage.  See [Clang documentation][] for
additional information.  See the `hs-bindgen` section below for details about
how to configure Clang via `hs-bindgen`.

[Clang documentation]: <https://clang.llvm.org/docs/index.html>

Clang maintains separate C include search paths.

The *bracket C include search path* takes precedence in resolution of bracket
includes.  Clang constructs it with the following order:

1. Directories specified via `-I` command-line options
2. Directories specified via `-isystem` command-line options
3. Directories specified via environment variables
4. Default include directories
5. Directories specified via `-idirafter` command-line options

The *quote C include search path* takes precedence in resolution of quote
includes.  Clang constructs it from `-iquote` command-line options.

### Clang command-line options

The following options are used to add a directory to a C include search path:

* `-I <directory>` adds a directory to the bracket C include search path.
* `-isystem <directory>` adds a directory to the bracket C include search path.
  Headers resolved from the directory are considered system headers.
* `-idirafter <directory>` adds a directory to the end of the bracket C include
  search path.  Headers resolved from the directory are considered system
  headers.
* `-iquote <directory>` adds a directory to the quote C include search path.

These options can be passed multiple times, and the directories are added to
the C include search paths in the same order.

The following options are used to disable default include directories:

* `-nostdinc` disables the default include directories (both the standard system
  directories and the compiler builtin directories).
* `-nostdlibinc` disables the standard system directories.
* `-nobuiltininc` disables the compiler builtin directories.

Reference:

* [Include path management](https://clang.llvm.org/docs/ClangCommandLineReference.html#include-path-management)

### Clang environment variables

Environment variable `C_INCLUDE_PATH` is a delimited list of directory paths to
be added to the bracket C include search path.  Environment variable `CPATH`
does the same.

Linux example:

```sh
C_INCLUDE_PATH=/some/dep/include:/other/dep/include
```

Windows example:

```sh
C_INCLUDE_PATH=C:\some\dep\include;C:\other\dep\include
```

These environment variables are used by other compilers as well, including GCC.
If you export these environment variables, then every execution of `clang` and
`gcc` in the environment will also use of them, so care must be taken.  For
example, putting a Clang compiler builtin directory in `C_INCLUDE_PATH` could
result in errors when running `gcc` in the same shell.

Note that there are also system-specific environment variables that may add to
the bracket C include search path.

### Clang header resolution

Header resolution for a bracket include is done by searching for the specified
header relative to the following directories:

1. Directories in the bracket C include search path, in order
2. Directories in the quote C include search path, in order

Header resolution for a quote include is done by searching for the specified
header relative to the following directories:

1. Directory containing the header currently being processed
2. Directories in the quote C include search path, in order
3. Directories in the bracket C include search path, in order
4. Current working directory

Quote includes are generally used for including headers within a C project.
Since the directory containing the header currently being processed is checked
first, one rarely has to configure a quote C include search path.  Bracket
includes are generally used for including headers of dependencies, including
standard headers.

## `hs-bindgen`

`hs-bindgen` uses `libclang` to resolve headers when generating bindings.  Note
that this is separate from compilation of the generated bindings, described in
the CAPI section below.

`hs-bindgen` passes Clang command-line options to `libclang`.  See
[Clang options][] for details.  Include options that `hs-bindgen` passes to
`libclang` are created based on the `hs-bindgen` command-line options and
environment variables, described below.  In addition, `libclang` process the
Clang environment variables, described above.

[Clang options]: <03-ClangOptions.md>

`libclang` constructs C include search paths like Clang, but it is generally
unable to correctly determine the builtin include directory.  When the builtin
include directory is not configured correctly, one often gets "`stddef.h` not
found" or similar include resolution errors.  By default, `hs-bindgen` attempts
to determine and configure the builtin include directory automatically so that
it does not have to be done manually.

`hs-bingden` has two modes for configuring the builtin include directory:

* Clang configuration (`clang`) determines the builtin include directory using
  the Clang compiler, which must match the version of `libclang` being used.
  `hs-bindgen` compares the version strings and only proceeds when there is a
  match.

    1. `$(${LLVM_PATH}/bin/clang -print-resource-dir)/include`
    2. `$($(${LLVM_CONFIG} --prefix)/bin/clang -print-resource-dir)/include`
    3. `$($(llvm-config --prefix)/bin/clang -print-resource-dir)/include`
    4. `$(clang -print-resource-dir)/include`

* Configuration can be disabled (`disable`).  In this case, the builtin include
  directory can be configured using CLI options or environment variables when
  needed.  Note that this option only affects `hs-bindgen` behavior; Clang
  options must be used to configure `libclang` configuration of default include
  directories.

> [!NOTE]
> Debian packages are patched so that `libclang` can correctly determine the
> builtin include directory.  Debian-based distributions (such as Ubuntu)
> therefore do not have builtin include directory issues (when using the
> distribution packages).  When `hs-bindgen` automatically configures the same
> directory, `libclang` ignores it as a duplicate.  It works without issue.

### `hs-bindgen` command-line options

`hs-bindgen-cli` provides the following include options:

* `-I <directory>` adds a directory to the bracket C include search path.
  (Clang option: `-I`)
* `--builtin-include-dir MODE` determines how the builtin include directory is
  configured.

Clang has many more include options, which may be passed via
`--clang-option-before`, `--clang-option`, or `--clang-option-after` options.
For example, Clang option `-idirafter` may be used to add a directory to the end
of the bracket C include search path.

```console
hs-bindgen-cli preprocess \
  --standard c23 \
  -I include \
  --clang-option="-idirafter/opt/acme-0.1.0/include" \
  --module Foo \
  --hs-output-dir src \
  foo.h
```

### `hs-bindgen` environment variables

As described in [Clang options][], environment variable
`BINDGEN_EXTRA_CLANG_ARGS` may be used to specify arbitrary Clang options.

Options that `hs-bindgen` passes to Clang, including those specified in
`BINDGEN_EXTRA_CLANG_ARGS` take precedence over Clang environment variables
such as `C_INCLUDE_PATH`.

Environment variable `BINDGEN_BUILTIN_INCLUDE_DIR` can be used to set the
builtin include directory configuration mode, described above.  Valid values
are `disable` and `clang`.

### `hs-bindgen` header resolution

Headers to translate are specified as arguments to `hs-bindgen-cli preprocess`
or Template Haskell `hashInclude` function calls.  In addition, headers are
specified in [Binding specifications][].  All of these headers are resolved
via `libclang` using bracket includes.

[Binding specifications]: <06-BindingSpecifications.md>

Headers must be specified as they would be in an include directive.  They are
generally relative to a directory in a C include search path, but note that they
are C syntax.  Forward slashes must be used as directory separators, even when
using Windows.  (Backslashes are interpreted as characters in a directory or
file name.)

For example, a best practice for Haskell projects is to store project C code in
`include` and `cbits` directories, where the `include` directory contains the
headers and the `cbits` directory contains the `.c` source.  That `include`
directory should be added to the bracket C include search path, and header
arguments should be relative to it.  The following command runs the preprocessor
on header `foo.h` in the `include` directory:

```console
hs-bindgen-cli preprocess -I include foo.h
```

This translates to include directive `#include <foo.h>`.  Header resolution
searches for `foo.h` in the bracket C include search path and finds it in the
`include` directory.

Note that `hs-bindgen-cli preprocess include/foo.h` does not work in general,
even though it is convenient to type with tab-completion.  It translates to
include directive `#include <include/foo.h>`.  Header resolution searches for
`include/foo.h` in the bracket C include search path, and it will not find
`foo.h` unless you add the current working directory to the bracket C include
search path.

A C include search path is used to search for C headers, so only adding
directories that contain C header files is best practice.  Adding a directory
that contains C source as well as C header files is suboptimal.  Adding a
directory that contains many types of files (such as a Haskell project
directory) is bad practice.  Command
`hs-bindgen-cli preprocess -I . include/foo.h` is therefore a hack, but it works
as long as `foo.h` (and any transitively included headers) do not rely on the
`include` directory being in the bracket C include search path.

### CAPI

When needed, `hs-bindgen` generates C code that is included in the generated
Haskell module.  When the generated module is compiled, GHC calls a C compiler
to compile the generated C code.  The compiler that GHC is configured to use is
called, which generally defaults to GCC on Linux and Clang on Windows.  Check
the "C compiler command" in the output of `ghc --info` to confirm which compiler
is used.

GHC passes `-I` options to the C compiler to configure the bracket C include
path.  These options are determined by configuration in the `.cabal` files for
the package and all transitive dependencies, the `cabal.project` and
`cabal.project.local` files for the project, and any command-line configuration
(passed via `cabal` for example).  The order of the `-I` options passed to the C
compiler is determined by GHC.  Environment variables supported by the C
compiler (such as `C_INCLUDE_PATH`) are also relevant.

Note that `hs-bindgen` configuration is not relevant.  Header resolution at this
stage is *not* affected by `hs-bindgen-cli` options or the
`BINDGEN_EXTRA_CLANG_ARGS` environment variable.

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

```console
clang -E -v - </dev/null
```

Similarly, the following command runs the GCC preprocessor and displays
information that includes the C include search paths used by GCC.  It takes
command-line options and environment variables into account, so you can test
configuration and confirm precedence.

```console
gcc -xc -E -v - </dev/null
```

The `hs-bindgen-cli info resolve-header` command may be used debug `hs-bindgen`
header resolution.  When experimenting with builtin include directory
configuration, it may be useful to show debug trace messages.

```console
hs-bindgen-cli info resolve-header -v4 stddef.h
```

The `hs-bindgen-cli info include-graph` command may be used to display the full
include graph for one or more headers, in [Mermaid][] syntax.

[Mermaid]: <https://mermaid.js.org/>

```console
hs-bindgen-cli info include-graph stdint.h
```

The `hs-bindgen-cli info libclang` command may be used to run `libclang` with
Clang options such as `-v`, to confirm the `libclang` version, C include search
paths, etc.

```console
hs-bindgen-cli info libclang --clang-option=-v
```
