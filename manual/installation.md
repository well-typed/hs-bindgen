# Installation

In addition to covering installation, this part of the manual provides
system-related details about using `hs-bindgen`.

We distinguish the following *phases* of installing and using `hs-bindgen`,
where *user bindings* refers to the bindings that `hs-bindgen` generates.

1. [Build `hs-bindgen`][t:build-hs-bindgen]
    * [Requirements][t:requirements]
    * [Get `hs-bindgen`][t:get-hs-bindgen]
    * [Configure `libclang-bindings`][t:configure-libclang-bindings]
    * [Configure `hs-bindgen`][t:configure-hs-bindgen]
    * [Build and test][t:build-and-test]
2. [Run `hs-bindgen`][t:run-hs-bindgen], generating user bindings
3. [Build the user project][t:build-the-user-project] that includes those
  bindings
4. [Run the user project][t:run-the-user-project]

When using the `hs-bindgen` Template Haskell API, the second and third steps are
done together.

This part of the manual also includes information about
[Clang vs. GCC][t:clang-vs-gcc] and
[common errors and solutions][t:common-errors-and-solutions].

## Build `hs-bindgen`
[t:build-hs-bindgen]: #build-hs-bindgen

### Requirements
[t:requirements]: #requirements

`hs-bindgen` requires the following:

* [GHC][] 9.2 through 9.14
* [Cabal][] 3.0 or later
* [LLVM/Clang][] 16 or later, current tested through LLVM/Clang 22

> [!NOTE]
> We maintain a [Nix flake][] for using `hs-bindgen` with Nix.  We also provide
> a [Nix-focused tutorial][hs-bindgen-tutorial-nix].

`hs-bindgen` uses LLVM/Clang to parse and introspect C headers, via the
`libclang` C API.  There are frequent releases, and a smoother experience is
more likely when using versions that we test against.  The `libclang` bindings
are implemented in the [`libclang-bindings`][libclang-bindings] package.  See
the [`libclang-bindings` manual][libclang-bindings:manual] for details about
installing LLVM/Clang.

> [!WARNING]
> Windows users are recommended to use the LLVM/Clang that is installed with
> recent versions of GHC.  LLVM/Clang versions installed from official releases
> or with Visual Studio target MSVC, which is not supported by GHC, and are
> therefore not suitable.

### Get `hs-bindgen`
[t:get-hs-bindgen]: #get-hs-bindgen

A Hackage package will be made available from the first official release.  Until
then, it is necessary to clone the repository.

```bash
$ git clone https://github.com/well-typed/hs-bindgen.git
$ cd hs-bindgen
```

`hs-bindgen` uses trunk-based development on the `main` branch.  Use the `HEAD`
of the `main` branch to use the latest commits, or checkout a tag to use a
well-tested snapshot of the project.  Example:

```bash
$ git checkout -b release-0.1-alpha2 release-0.1-alpha2
```

### Configure `libclang-bindings`
[t:configure-libclang-bindings]: #configure-libclang-bindings

You can try out `hs-bindgen` without any special configuration as long as you
have the `llvm-config` (`llvm-config.exe` on Windows) and `clang` (`clang.exe`
on Windows) executables in your `PATH`.  You can confirm this as follows:

```bash
$ which llvm-config
$ which clang
$ llvm-config --version
```

Special configuration in a `cabal.project.local` file may be required to work
around some Cabal linking issues, however, if you want to test against multiple
versions of LLVM/Clang or even upgrade the version of LLVM/Clang that you use.
See the [`libclang-bindings` manual][libclang-bindings:manual] for details.

### Configure `hs-bindgen`
[t:configure-hs-bindgen]: #configure-hs-bindgen

The `hs-bindgen` library and `hs-bindgen-cli` executable do not require any
additional configuration to be built.  When building the tests, however, the
system include directories must be determined.

By default, `hs-bindgen` uses the `clang` executable to determine the system
include directories, so nothing needs be configured manually.  This is described
in the
[`hs-bindgen` section of the includes documentation][manual:includes-hs-bindgen].
In cases where this does not work, the `BINDGEN_EXTRA_CLANG_ARGS` environment
variable can be used to configure Clang options that `hs-bindgen` always
applies.  This is described in the
[environment variables section of the Clang options documentation][manual:clang-options-env].

On macOS, the `SDKROOT` environment variable must be set to instruct LLVM/Clang
which SDK to use.  The default can generally be determined using
`xcrun --sdk macosx --show-sdk-path`.  If `SDKROOT` is not set, `hs-bindgen`
attempts to run this command and automatically set it to the default.

### Build and test
[t:build-and-test]: #build-and-test

Build the `hs-bindgen` library, the `hs-bindgen-cli` executable, and all tests
as follows.

```bash
$ cabal build all
```

If you would like to run the test suites, that can be done as follows.

```bash
$ cabal test all
```

## Run `hs-bindgen`
[t:run-hs-bindgen]: #run-hs-bindgen

When generating bindings, `hs-bindgen` must be able to find the specified
headers as well as any headers that are transitively included by those specified
headers.  This is typically done using `-I` options to the CLI or `hashInclude`
calls when using Template Haskell.

In cases when you are generating bindings for software that is installed on your
system and the headers are in a system include directory (such as
`/usr/include`), then you generally do not need to configure header directories.

It is possible to configure header directories using the `C_INCLUDE_PATH` or
`BINDGEN_EXTRA_CLANG_ARGS` environment variables, as described in the
[Clang options documentation][manual:clang-options].  While such environment
variables are helpful for configuring general system paths, they are not
recommended for configuring project-specific paths.

See the [includes documentation][manual:includes] for details.

## Build the user project
[t:build-the-user-project]: #build-the-user-project

To build the user project that includes generated user bindings, GHC must be
configured so that it can find the headers as well as libraries.  This is done
in different ways, depending on the software that you are creating bindings for.

For software that has [`pkg-config`][pkg-config] support, the
`.cabal` file should be configured using
[`pkgconfig-depends`][cabal:docs:pkgconfig-depends].  (In this case, using
`pkg-config` to get the include directories to configure `hs-bindgen` ensures
consistency.)  Example:

```cabal
executable example
  ...
  pkgconfig-depends:
      acme
  ...
```

For software that lacks `pkg-config` support, the `.cabal` file can be
configured using [`include-dirs`][cabal:docs:include-dirs] and
[`extra-lib-dirs`][cabal:docs:extra-lib-dirs].  Example:

```cabal
executable example
  ...
  include-dirs:
      /path/to/acme/include
  extra-lib-dirs:
      /path/to/acme/lib
  ...
```

In cases where the paths are specific to each system, the `cabal.project.local`
file can be configured using
[`extra-include-dirs`][cabal:docs:project/extra-include-dirs] and
[`extra-lib-dirs`][cabal:docs:project/extra-lib-dirs] instead.  Example:

```cabal
package manual
  extra-include-dirs:
      /path/to/hs-bindgen/manual/c
  extra-lib-dirs:
      /path/to/hs-bindgen/manual/c
```

## Run the user project
[t:run-the-user-project]: #run-the-user-project

To run the user project, the loader needs to be able to find any linked
libraries.

In cases when you are generating bindings for software that is installed on your
system and the libraries are in a system library directory (such as `/usr/lib`),
then you generally do not need to configure the library directory.

If you linked against a library in a non-standard directory, however, you may
need to configure the library directory.  This is done differently on different
operating systems.

### Linux

You can configure a loader library directory using the `LD_LIBRARY_PATH`
environment variable.  Example:

```bash
$ export LD_LIBRARY_PATH=/path/to/your/project/lib:$LD_LIBRARY_PATH
```

> [!NOTE]
> Configure the directory that contains the `.so` file, not the `.so` file
> itself.

### macOS

You can configure a loader library directory using the `DYLD_LIBRARY_PATH`
environment variable.  Example:

```bash
$ export DYLD_LIBRARY_PATH=/path/to/your/project/lib:$DYLD_LIBRARY_PATH
```

> [!NOTE]
> Configure the directory that contains the `.dylib` file, not the `.dylib` file
> itself.

### Windows

Windows searches for libraries (DLLs) using the `PATH` environment variable.
This is the same `PATH` that is used to search for executables, and it is common
to install libraries in the same directory as executables.  Configure your
`PATH` so that both the executable and libraries are found.  Example:

```powershell
$env:Path = "C:\path\to\your\project\bin;" + $env:Path
```

## Clang vs. GCC
[t:clang-vs-gcc]: #clang-vs-gcc

Since `hs-bindgen` uses [LLVM/Clang][] to parse and introspect C headers, using
Clang to compile C code ensures compatibility.  There are many cases where GCC
is used, however.  In practice, Clang is highly ABI-compatible with GCC.  While
we have not observed any incompatibility issues yet, such issues are possible.

GHC is built using a specific C compiler, generally either Clang or GCC.  GHC
uses this C compiler to build any C code that is part of the Haskell package,
including `static inline` functions in C headers as well as wrapper functions
such as those generated by `hs-bindgen` to support APIs that pass `struct`s by
value.

You can check which C compiler a specific build of GHC uses by running
`ghc --info` and looking at the "C compiler command."  When using a build of GHC
that uses GCC, it is *not* straightforward to configure it to use Clang instead.
There are [GHC program options][ghc:guide:phases-programs] such as `-pgmc` and
`-pgml` that look promising, but experiments indicate that setting such options
is insufficient.  If you would like to use Clang with GHC to ensure
compatibility, and no GHC build that uses Clang is already available, building
GHC with Clang is recommended.

When linking to existing shared libraries, such as those that are installed on
your system via a package manager, those shared libraries may have been built
using GCC.  If you would like to only use Clang to ensure compatibility, you
need to (re)build such libraries using Clang.

This is a problem that Nix should solve.  Beware, however, that Nix uses GCC by
default.

## Common errors and solutions
[t:common-errors-and-solutions]: #common-errors-and-solutions

### LLVM/Clang version mismatch
[t:llvm-clang-version-mismatch]: #llvmclang-version-mismatch

Three LLVM/Clang versions are relevant:

* **Clang compile time version**: The `libclang-bindings` library is built
  using a specific version of LLVM/Clang, as described in the
  [`libclang-bindings` manual][libclang-bindings:manual].
* **Clang runtime version**: The `libclang-bindings` library, `hs-bindgen`
  libraries, and `hs-bindgen-cli` executable are linked to the `libclang` shared
  library.  When running `hs-bindgen`, this shared library is loaded.
* **Clang executable version**: In addition, `hs-bindgen` uses the `clang`
  executable to get information that is not available via the `libclang` C API,
  when the executable is available.  It searches for the `clang` executable
  using the same logic used to configure `libclang-bindings`.

The major/minor version of all of these should be consistent.  There should be
warnings/errors if there are any inconsistencies, but you can also check for
inconsistencies on the command line.  Example:

```bash
$ cabal run hs-bindgen-cli -- --version
hs-bindgen 0.1.0
binding specification 1.0
clang compile time version: clang version 21.1.8
clang runtime version:      clang version 21.1.8
$ clang --version
clang version 21.1.8
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/lib/llvm21/bin
```

See the [`libclang-bindings` manual][libclang-bindings:manual] for details about
configuring `libclang-bindings`.  First, ensure that your environment matches
the environment used to build `libclang-bindings`.  If you have upgraded
LLVM/Clang or want to test against multiple versions, note that you may need
special configuration to work around Cabal issues.

### Missing standard header
[t:missing-standard-header]: #missing-standard-header

Errors about missing standard headers (such as `stddef.h`) when generating
bindings indicate that the system include directories are not being configured
correctly.  As described in the [Configure `hs-bindgen`][t:configure-hs-bindgen]
section above, `hs-bindgen` uses the `clang` executable to determine the system
include directories by default.  Such errors may occur when the `clang`
executable does not match the `libclang` library.  See
[LLVM/Clang version mismatch][t:llvm-clang-version-mismatch] above for
information about this case.

As a last resort, you may be able to work around such issues using the
`BINDGEN_EXTRA_CLANG_ARGS` environment variable.  Configure it with `-nostdinc`
and `-I` options to effectively override all system include directories.
Example:

```bash
$ export BINDGEN_EXTRA_CLANG_ARGS="-nostdinc -I/usr/lib/clang/21/include -I/usr/include"
```

There are details about debugging include issues in the
[includes documentation][manual:includes].

### Missing shared libraries
[t:missing-shared-libraries]: #missing-shared-libraries

An error like `cannot open shared object file: No such file or directory`
indicates that a linked library cannot be loaded.

> [!WARNING]
> Windows generally does not display an error when a DLL cannot be found.  It
> fails silently, with a negative exit code, making the issue more difficult to
> diagnose.

You can use the `ldd` command to determine which libraries are linked to an
executable or library, as well as determine which cannot be loaded.

If a `libclang` shared library is missing, see the
[`libclang-bindings` manual][libclang-bindings:manual] for details about
configuring `libclang-bindings`.  First, ensure that your environment matches
the environment used to build `libclang-bindings`.  If you have upgraded
LLVM/Clang or want to test against multiple versions, note that you may need
special configuration to work around Cabal issues.

If a different library is missing, see the
[run the user project][t:run-the-user-project] section above for details about
configuring loader library directories.

### Underlying type mismatches
[t:underlying type mismatches]: #underlying-type-mismatches

You might encounter Haskell type errors where, for example, a C `int` is being
interpreted as a `CUInt` instead of a `CInt`.  This is often due to how
different compilers and platforms define basic types.  Carefully check your C
and Haskell type definitions to ensure they match.

### Unicode errors
[t:unicode-errors]: #unicode-errors

Various systems do not support Unicode identifiers:

* The GHC LLVM backend does not support Unicode.
* The macOS assembler does not support Unicode.
* The Windows assembler does not support Unicode.

Avoid using Unicode-specific characters in C function definitions to work around
such limitations.



<!-- sources and references -->

[Cabal]: https://www.haskell.org/cabal/
[cabal:docs:extra-lib-dirs]: https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#pkg-field-extra-lib-dirs
[cabal:docs:include-dirs]: https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#pkg-field-include-dirs
[cabal:docs:pkgconfig-depends]: https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#pkg-field-pkgconfig-depends
[cabal:docs:project/extra-include-dirs]: https://cabal.readthedocs.io/en/stable/cabal-project-description-file.html#cfg-field-extra-include-dirs
[cabal:docs:project/extra-lib-dirs]: https://cabal.readthedocs.io/en/stable/cabal-project-description-file.html#cfg-field-extra-lib-dirs
[GHC]: https://www.haskell.org/ghc/
[ghc:guide:phases-programs]: https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#replacing-the-program-for-one-or-more-phases
[hs-bindgen-tutorial-nix]: https://github.com/well-typed/hs-bindgen-tutorial-nix
[libclang-bindings]: https://github.com/well-typed/libclang-bindings
[libclang-bindings:manual]: https://github.com/well-typed/libclang-bindings/blob/main/manual/README.md
[LLVM/Clang]: https://github.com/llvm/llvm-project
[manual:clang-options]: low-level/usage/clang-options.md
[manual:clang-options-env]: low-level/usage/clang-options.md#environment-variables
[manual:includes]: low-level/usage/includes.md
[manual:includes-hs-bindgen]: low-level/usage/includes.md#hs-bindgen
[Nix flake]: ../flake.nix
[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config
