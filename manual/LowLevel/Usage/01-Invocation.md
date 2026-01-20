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
cabal run hs-bindgen-cli -- preprocess [OPTIONS] HEADER_FILE
```

On Windows:

```powershell
cabal run hs-bindgen-cli.exe -- preprocess [OPTIONS] HEADER_FILE
```

### Options

The `preprocess` command accepts several categories of options.

#### Module generation

Options controlling module generation:

- `--hs-output-dir DIR` - Output directory for generated modules
- `--module NAME` - Base module name (e.g., `Generated.MyLib`)
- `--unique-id ID` - Unique identifier for C wrapper functions (e.g., `org.example.mylib`)
- `--create-output-dirs` - Create output directories if they do not exist

#### Clang configuration

Options configuring `libclang`:

- `-I DIR` - Add include directory
- `--gnu` - Enable GNU extensions
- `--c-standard STANDARD` - Specify C standard (c89, c99, c11, c17)
- `--clang-option OPT` - Pass arbitrary option to Clang
- `--clang-option-before OPT` - Pass option before managed options
- `--clang-option-after OPT` - Pass option after managed options

See [Clang options](03-ClangOptions.md) for details about the order in which
options are passed to Clang.

#### Parse predicates

Options determining which declarations are parsed:

- `--parse-from-main-header-dirs` - Parse all headers in main header directory
- `--parse-by-header-path PATTERN` - Parse declarations from headers matching pattern
- `--parse-by-decl-name PATTERN` - Parse declarations with names matching pattern

Note that `hs-bindgen` always parses declarations required for scoping (e.g.,
type definitions), regardless of parse predicates.  These declarations are
filtered during selection.

See [Parsing, selecting, and program slicing](05-ParsingSelectingAndProgramSlicing.md) for details.

#### Select predicates

Options determining which parsed declarations are included in the generated
bindings:

- `--select-by-header-path PATTERN` - Select declarations from headers matching pattern
- `--select-by-decl-name PATTERN` - Select declarations with names matching pattern
- `--select-except-by-decl-name PATTERN` - Exclude declarations with names matching pattern
- `--select-except-deprecated` - Exclude deprecated declarations
- `--enable-program-slicing` - Enable program slicing (includes transitive dependencies)

With program slicing disabled (the default), only declarations matching select
predicates are included.  With program slicing enabled, transitive
dependencies of selected declarations are included, even if explicitly
deselected.

See [Parsing, selecting, and program slicing](05-ParsingSelectingAndProgramSlicing.md)
for details.

### Example

The following example is adapted from `examples/libpcap/generate.sh`:

```bash
cabal run hs-bindgen-cli -- preprocess \
    -I "./libpcap" \
    --unique-id org.hs-bindgen.libpcap \
    --hs-output-dir hs-project/src \
    --create-output-dirs \
    --module Generated.Pcap \
    --gnu \
    --parse-by-header-path "struct_timeval.h" \
    --parse-by-header-path "socket.h" \
    --parse-from-main-header-dirs \
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

Run `cabal run hs-bindgen-cli -- --help` for details.

### Exit codes

`hs-bindgen` uses the following exit codes:
- 0: Success
- 1: Other errors (panics)
- 2: Invocation of `libclang` has failed
- 3: An `hs-bindgen`-specific error has happened

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

The Template Haskell mode (described later) avoids the need for any of these
approaches, but is less suitable for cross-compilation scenarios where target
platform information may not be available at compile time.

A minimal demonstration of the literate preprocessor mechanism (independent of
hs-bindgen) is available [here](../../../examples/literate-example).

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

The `.lhs` file can contain arbitrary content—it is simply passed to the
preprocessor.  The preprocessor is responsible for parsing the file and
generating Haskell code.

### Build process

When `cabal build` is invoked:

 1. Cabal detects the `.lhs` file
2. Cabal invokes `hs-bindgen-cli tool-support literate src/MyBindings.lhs
   src/MyBindings.hs -I ./c-lib --module=MyBindings --unique-id org.example.mybindings --gnu --enable-program-slicing mylib.h`
3. The preprocessor reads the configuration from the file
4. The preprocessor generates bindings, equivalent to `hs-bindgen-cli
   preprocess`
5. The preprocessor writes the generated code to `src/MyBindings.hs`
6. GHC compiles the resulting `.hs` file

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
    cfg = def & #clang % #extraIncludeDirs .~ [Pkg "my-c-lib"]

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
  - `Pkg "package"` - Package directory
  - `Abs "/path"` - Absolute path
  - `Rel "path"` - Relative path (relative to module directory)
- `#clang % #gnu` - GNU extensions (`GnuEnabled` or `GnuDisabled`)
- `#clang % #cStandard` - C standard (e.g., `C99`, `C11`)
- `#parse` - Parse predicates
- `#select` - Select predicates and program slicing
- `#backend % #safety` - Safety (`Safe`, `Unsafe`, or `GenerateBoth`)

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
                  Pkg "my-c-library"
                , Abs "/usr/local/include"
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

### Troubleshooting

**"Not in scope" errors:** Verify that `{-# LANGUAGE TemplateHaskell #-}` is
enabled, `HsBindgen.TH` is imported, and `hs-bindgen` is in `build-depends`.

**"Could not find header" errors:** Check include directories in `#clang %
#extraIncludeDirs`.  Try absolute paths if relative paths fail.

**Long compilation times:** Reduce selected declarations via predicates, or
use command-line or preprocessor invocation instead.  Use higher verbosity
(`-v3`) to see what is being processed.

## Preparation of system environment for `hs-bindgen`

Depending on the platform, specific steps ensure a smooth experience with
`hs-bindgen`. This guide will walk you through the setup process for Linux,
macOS, and Windows.

### Linux

Setup on Linux involves choosing between the GCC and Clang compiler
toolchains.

>[!NOTE]
>
> We also provide and maintain [Nix Flake](../../../flake.nix), and a
> [Nix-focused tutorial](https://github.com/well-typed/hs-bindgen-tutorial-nix).

#### Compiler choice (GCC vs. Clang)

GCC is the default compiler on many Linux distributions and is also the compiler
used by GHC. However, `hs-bindgen` uses `libclang` internally to parse C code.
GCC and Clang will mostly be compatible, but there are some exceptions. For
example, GCC and Clang may exhibit differences in memory layout. Unfortunately,
it is not straightforward to make GHC use Clang without effectively building GHC
from scratch. There are flags like `-pgmc` and `-pgml` ([see the GHC user
guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#replacing-the-program-for-one-or-more-phases))
which allow specification of the C compiler used by GHC; however, experiments
showed that just setting these flags was not enough. For more details see the
discussions at issues
[#846](https://github.com/well-typed/hs-bindgen/issues/846), and
[#847](https://github.com/well-typed/hs-bindgen/issues/847).

Using Clang for both, generation of and compilation of the bindings ensures
consistency and correctness, but we currently do not thoroughly test compilation
of code using the generated bindings with Clang.

#### Installation of LLVM and Clang

You need to install LLVM and Clang. The specific package names may vary
depending on your distribution.

#### Adding `cabal.project.local`

We need to inform the program about the location of the C sources. We can do so
by creating a `cabal.project.local` file that defines `extra-include-dirs` and
`extra-lib-dirs`.

For example, to build the manual you will need the following:

```cabal
package manual
  extra-include-dirs:
      <path-to-hs-bindgen>/manual/c
  extra-lib-dirs:
      <path-to-hs-bindgen>/manual/c

package hs-game
  extra-include-dirs:
      <path-to-hs-bindgen>/manual/c
  extra-lib-dirs:
      <path-to-hs-bindgen>/manual/c

package hs-vector
  extra-include-dirs:
      <path-to-hs-bindgen>/manual/c
  extra-lib-dirs:
      <path-to-hs-bindgen>/manual/c
```

Note that the example above enables compilation of the manual from this
repository. For a custom project, `extra-include-dirs` and `extra-lib-dirs`
should be set in the `.cabal` file.

#### Setting environment variables

* `LD_LIBRARY_PATH`: Ensures that the C libraries you build can be linked at
  runtime, you need to add their locations to this variable.

  * Example: If your shared library `libexample.so` is in
    `/path/to/your/c/libs`, run:

    ```bash
    export LD_LIBRARY_PATH=/path/to/your/c/libs:$LD_LIBRARY_PATH
    ```

* `BINDGEN_EXTRA_CLANG_ARGS`: Contains extra arguments passed to `libclang`.
  This is particularly useful for specifying include directories.

  * Find your system's default include paths with

    ```bash
    clang -v -E -xc /dev/null
    ```

  * Set the variable accordingly,

    ```bash
    export BINDGEN_EXTRA_CLANG_ARGS="-nostdinc -I/usr/lib/clang/14/include -I/usr/include"
    ```

    > [!NOTE]
    > Without `-nostdinc`, there are likely multiple directories in the C
    > include path that provide the same headers. Which header is ultimately
    > used depends on the order of directories in the include path.

  * Note that the common use of this environment variable is to set preprocessor
    flags. Include paths can be directly set with the `-I` flag of
    `hs-bindgen-cli`.

* `LLVM_PATH`, `LLVM_CONFIG`: `hs-bindgen` may need to know where to find
  your LLVM installation.

  ```bash
  export LLVM_PATH=/usr/lib/llvm-14
  export LLVM_CONFIG=$LLVM_PATH/bin/llvm-config
  ```

  This is only needed when you want to use a version of LLVM/Clang that is
  not in your current `PATH`.

#### Common errors and solutions

* Missing headers (`stddef.h`, etc.): If you encounter errors about missing
  standard headers, `libclang` cannot find the system include directories. Try
  setting `BINDGEN_EXTRA_CLANG_ARGS` as described above.

* Missing shared libraries (`libexample.so`): If you see an error like `cannot
  open shared object file: No such file or directory`, it means the dynamic
  linker cannot find your C library. Try adding the directory of the library to
  `LD_LIBRARY_PATH` or adapting your `cabal.project.local`.

* Unicode character issues with LLVM backend: When using the LLVM backend,
  you might encounter issues with Unicode characters in your C code. This can
  sometimes manifest as errors at the assembler level. Ensure your source
  files do not include Unicode.

### macOS

On macOS, the setup is similar to Linux, but with its own specific environment
variables and considerations, especially concerning the system's default Clang
installation. Note that Apple's assembler does not support Unicode, so avoid
using Unicode-specific characters in C function definitions.

#### Environment variables

* `DYLD_LIBRARY_PATH`: This is the macOS equivalent of `LD_LIBRARY_PATH`. It
  tells the dynamic linker where to find dynamic libraries (`.dylib` files).

  * Example:

    ```bash
    export DYLD_LIBRARY_PATH=/path/to/your/c/libs:$DYLD_LIBRARY_PATH
    ```

* `BINDGEN_EXTRA_CLANG_ARGS`: On macOS, setting the include paths like we
  suggest for Linux is not required. If you need to, see the corresponding
  section for Linux.

### Windows

Setup on Windows involves handling of Dynamic-Link Libraries (DLLs) and the
configuration of the compiler environment.

#### Built-in Clang with GHC installation

When you install GHC on Windows using GHCup, for example, it comes with a MinGW
environment that includes LLVM and Clang. Windows' assembler does not support
Unicode, so avoid using Unicode-specific characters in C function definitions.

#### Explicit handling of `PATH` for DLLs

At runtime, Windows primarily finds DLLs by searching the directories listed in
the `PATH` environment variable. This is a crucial difference from Linux and
MacOS.

* To ensure your application can find the required DLLs, add the corresponding
  directories to the `PATH`:

  ```powershell
  $env:PATH = "C:\path\to\your\c\libs;" + $env:PATH
  ```

#### Environment Variables

* `LLVM_PATH`, `LLVM_CONFIG`, `LIBCLANG_PATH`: You need to point `hs-bindgen`
  to the LLVM/Clang installation that comes with GHC.

  ```powershell
  $env:LLVM_PATH = "C:\ghcup\ghc\<your-ghc-version>\mingw"
  $env:LLVM_CONFIG = "$env:LLVM_PATH\bin\llvm-config.exe"
  $env:LIBCLANG_PATH = "$env:LLVM_PATH\lib"
  ```


* `BINDGEN_EXTRA_CLANG_ARGS`: On Windows, setting the include paths like we
  suggest for Linux is not required. If you need to, see the corresponding
  section for Linux.

#### Common Errors and Solutions

* Dynamic-link library loading order: If your application fails silently or with
  an `ExitFailure` and a cryptic error code like `(-1073741515)`, it is very
  likely a DLL loading issue. Try adding the directory containing the C DLLs of
  your library to the system `PATH`.

* Resolving issues with underlying type mismatches (`FC.CInt` vs.
  `FC.CUInt`): You might encounter Haskell type errors where, for example, a C
  `int` is being interpreted as a `CUInt` instead of a `CInt`. This is often
  due to how different compilers and platforms define basic types. Carefully
  check your C and Haskell type definitions to ensure they match.

  Bindings generated by `hs-bindgen` are not portable. In order to create a
  portable API, one must do so at a higher level, using CPP to create an
  abstraction layer over the low-level, platform-specific bindings.

  Given that `hs-bindgen` only supports generating bindings for a subset of
  targets people use Hackage with, perhaps all generated bindings uploaded to
  Hackage should include appropriate gates. Minimal example:

  ```
  if !(os(linux) && arch(x86_64))
    buildable: false
  ```
