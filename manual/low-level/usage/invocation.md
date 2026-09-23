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

## Library mode
[t:library-mode]: #library-mode

When `--library DIR` is passed, `preprocess` switches to library mode: it
walks the include graph of the root header(s), assigns each discovered
sub-header its own Haskell module, and runs the binding generator once per
module in dependency order. Each step receives the binding specifications from
all previous steps as external binding specifications, so cross-module type
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

1. Parses `rpm/rpmlib.h` (resolved via `-I /usr/include`), which transitively
   includes all RPM public headers.
2. Walks the include graph to discover every header reachable from the root.
3. Filters headers to those under `--library /usr/include/rpm`.
4. Topologically sorts the filtered headers.
5. For each header, derives a Haskell module name, constructs a selection
   predicate targeting that header's declarations, enables program slicing, and
   runs the binding generator.
6. Chains binding specifications: each step receives the binding specifications
   from all previous steps as external binding specifications, so cross-module
   type references resolve.

### Module-generation scope

`--library DIR` defines which headers get their own Haskell module. A header
in the include graph gets a module if and only if its normalised path falls
under a library directory.

`--except-library PCRE` excludes headers whose normalised path matches the
pattern, even when they are under a library directory. Types from excluded
headers remain available to other modules through program slicing and binding
spec chaining.

```
hs-bindgen-cli preprocess \
    --library /usr/include/rpm \
    --except-library 'internal' \
    ...
```

This skips any header whose path contains "internal" (e.g.
`rpm/internal.h`).

Note: `-I` is the clang search path only; it tells clang where to find headers
during parsing and has no effect on which headers get modules.

### Selection predicates in library mode

Selection predicates (`--select-by-header-path`, `--select-by-decl-name`,
`--select-except-deprecated`, etc.) control which *declarations* get bindings
within each generated module.  They are independent of `--library` and
`--except-library`, which control which *headers* get modules.

In library mode the selection predicate defaults to all declarations,
rather than only the main headers as in single-header, preprocess mode.

### Module naming

Module names are derived from each header's normalised path relative to the
`--library` directories (which are normalised before matching, so symlinks and
`..` segments are resolved).

| `--library` | Header path | Module name |
|---|---|---|
| `/usr/include/rpm` | `/usr/include/rpm/rpmlib.h` | `RPM.Rpmlib` |
| `/usr/include/rpm` | `/usr/include/rpm/rpmtypes.h` | `RPM.Rpmtypes` |
| `/usr/include` | `/usr/include/rpm/argv.h` | `RPM.Rpm.Argv` |

### Dry run and module listing

`--dry-run` prints the processing plan (which headers produce which modules)
and exits without generating any files. Useful for verifying the `--library`
and `--except-library` filters before running the full generation.

`--list-modules` prints module names one per line (suitable for pasting into a
`.cabal` file) and exits.

### Module name collisions

The naming scheme can produce collisions. Library mode detects them before
generating any files and exits with an error. Known cases:

- Two headers that differ only in the capitalisation of the first character
  collide: `foo.h` and `Foo.h` both produce component `Foo`.

- Only the last file extension is stripped, so a header like `Widget.Core.h`
  retains a dot in the stem (`Widget.Core`). That dot becomes a module
  separator in the derived name, producing the same module as `widget/core.h`
  would from two separate path components. Any dot that survives extension
  stripping is indistinguishable from a directory separator.

- Each base module `M` can expand to category submodules `M.Safe`, `M.Unsafe`,
  `M.FunPtr`, and `M.Global`. So, for example, `foo.h` (module `M.Foo`) and
  `foo/safe.h` (module `M.Foo.Safe`) collide at `M/Foo/Safe.hs`.

To resolve a collision, use `--except-library` to exclude one side, or
adjust the `--library` directories to change the derived relative paths.

For now the `--library` mode does not allow to overwrite the name of each
individually generated header module.

[manual:binding-specifications-multi]: binding-specifications.md#generating-multiple-modules

### Exit codes

`hs-bindgen` uses the following exit codes:

- 0: Success
- 1: Unexpected errors (panics)
- 2: CLI usage errors
- 3: Invocation of `libclang` has failed
- 4: An `hs-bindgen`-specific error has happened

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
[manual:c-stages]: c-stages.md
[manual:clang-options]: clang-options.md
[manual:installation]: ../../installation.md
[manual:installation-clang-vs-gcc]: ../../installation.md#clang-vs-gcc
[manual:macros]: ../translation/macros.md
[manual:selecting-and-program-slicing]: selecting-and-program-slicing.md
