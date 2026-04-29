# Installation

Depending on the system, specific steps ensure a smooth experience with
`hs-bindgen`. This guide will walk you through the setup process for Linux,
macOS, and Windows.

## Linux

We recommend using the Clang compiler toolchain on Linux for best compatibility
with `hs-bindgen`. GCC should normally work, but there are edge cases where GCC
and Clang differ.

### Compiler choice (GCC vs. Clang)

`hs-bindgen` uses `libclang` internally to parse C code. GCC is the default
compiler on many Linux distributions and is also the compiler used by GHC.
While GCC and Clang are mostly compatible, they can exhibit differences in
memory layout for some constructs. Using Clang for both generation of and
compilation of the bindings ensures consistency and correctness.

Note that `static inline` functions in a C header file are compiled using the C
compiler that GHC is configured to use. One should beware of any Clang/GCC
discrepancies in such functions when GCC is used.

Unfortunately, it is not straightforward to make GHC use Clang without
effectively building GHC from scratch. There are flags like `-pgmc` and `-pgml`
([see the GHC user
guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#replacing-the-program-for-one-or-more-phases))
which allow specification of the C compiler used by GHC; however, experiments
showed that just setting these flags was not enough. For more details see the
discussions at issues
[#846](https://github.com/well-typed/hs-bindgen/issues/846), and
[#847](https://github.com/well-typed/hs-bindgen/issues/847).

Using Clang for both, generation of and compilation of the bindings ensures
consistency and correctness, but we currently do not thoroughly test compilation
of code using the generated bindings with Clang.

### Installation of LLVM and Clang

You need to install LLVM and Clang. The specific package names may vary
depending on your distribution.

### Adding `cabal.project.local`

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

### Setting environment variables

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

### Cabal store maintenance

The `libclang` bindings used by `hs-bindgen` are implemented in package
`libclang-bindings`.  When built, it is linked to the `libclang` and `libLLVM`
shared objects for the version of LLVM/Clang that you are using, and the built
`libclang-bindings` library is cached in the Cabal store.  Cabal does not
consider such system dependencies to determine when a package needs to be
rebuilt, however, so you might continue to use a library built with an older
version of LLVM/Clang even after a system upgrade installs a newer version.

Links to LLVM/Clang shared objects generally specify the minor version.  If an
upgrade replaces an older version of LLVM/Clang with a newer version that has
the same minor version (and a newer patch version), then the link in the
cached `libclang-bindings` build is not broken.  The newer version *should*
still work, though we recommend rebuilding `hs-bindgen`/`libclang-bindings`
using the newer version.  In this case, `hs-bindgen` displays a warning like
the following, where the "compile time version" is the version of LLVM/Clang
used to to build the cached `libclang-bindings` library, and the "runtime
version" is the version for the shared object that is dynamically linked at
runtime.

```
[Warning] [HsBindgen] [boot] clang version mismatch:
  clang compile time version: 22.1.2
  clang runtime version:      22.1.3
```

If an upgrade installs a new major version and removes the older version, then
the link in the cached `libclang-bindings` build is broken.  In this case, the
dynamic library loader fails with an error like the following, preventing
`hs-bindgen` from running at all.

```
libclang.so.20.1: cannot open shared object file: No such file or directory
```

Cabal does not provide an easy way to resolve this issue.  Note that this is an
issue for any Haskell package that links to system libraries that do not have
`pkg-config` support (in which case Cabal can track the dependency versions).
It is particularly problematic with LLVM/Clang, however, because LLVM/Clang has
frequent releases.  Currently, we suggest two ways to work around the issue.

The easiest way to force Cabal to rebuild out-of-date/broken packages is to
clear the cache.  You can determine your Cabal store directory by running
`cabal path --store-dir`.  The Cabal store has separate caches for each version
of GHC.  Recursively remove a GHC directory to clear the cache for that version.
Note that deleting just a package directory is not advised because it can break
the package database.  The downside to clearing your cache is that doing so may
result in time-consuming recompilation of many other packages.

Since we understand that clearing the cache might not be desirable, `hs-bindgen`
offers a bespoke workaround, in the form of a compile-time setting specifying
the LLVM/Clang version.  This forces Cabal to distinguish `libclang-bindings`
builds that link to different versions, forcing a new build if one for the
specified version is not already in the Cabal store.  Do this by adding the
following to a `cabal.project.local` file for your project, with the desired
version.

```
package libclang-bindings
  ghc-options: -optc=-DCLANG_VERSION=22.1.3
```

When configured like this, `libclang-bindings` confirms that the version matches
the version of LLVM/Clang used at compile time.  The following checks are
supported:

* `MAJOR` to just check the major version (example: `22`)
* `MAJOR.MINOR` to check the major and minor versions (example: `22.1`)
* `MAJOR.MINOR.PATCH` to check the major, minor, and patch versions
  (example: `22.1.3`)

Note that `ghc-options` specifying an `-optc` option must be used, as
`cc-options` is *not* sufficient.

This issue affects any project that uses `hs-bindgen`, even if it is used as a
transitive dependency.

### Common errors and solutions

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

## macOS

On macOS, the setup is similar to Linux, but with its own specific environment
variables and considerations, especially concerning the system's default Clang
installation. Note that Apple's assembler does not support Unicode, so avoid
using Unicode-specific characters in C function definitions.

### Environment variables

* `SDKROOT`: This environment variable must be set to instruct LLVM/Clang which
  SDK to use.  The default can generally be determined using
  `xcrun --sdk macosx --show-sdk-path`.  If `SDKROOT` is not set, `hs-bindgen`
  attempts to run this command and automatically set it to the default.

* `DYLD_LIBRARY_PATH`: This is the macOS equivalent of `LD_LIBRARY_PATH`. It
  tells the dynamic linker where to find dynamic libraries (`.dylib` files).

  Example:

  ```bash
  export DYLD_LIBRARY_PATH=/path/to/your/c/libs:$DYLD_LIBRARY_PATH
  ```

* `BINDGEN_EXTRA_CLANG_ARGS`: On macOS, setting the include paths like we
  suggest for Linux is not required. If you need to, see the corresponding
  section for Linux.

## Windows

MSVC, the default Windows toolchain, is currently not supported by GHC.  GHC is
therefore distributed with a MinGW environment, which includes a GNU standard
library.

### LLVM/Clang

Official distributions of GHC 9.4 and later use Clang as the C compiler.  The
easiest way to use `hs-bindgen` on Windows is to use the LLVM/Clang installation
that is distributed with GHC.

Since GHC does not support MSVC, LLVM/Clang installed by Visual Studio or from
the LLVM project releases should not be used.

### Environment Variables

* `LLVM_PATH`, `LLVM_CONFIG`, `LIBCLANG_PATH`: These environment variables
  configure which LLVM/Clang installation is used by `hs-bindgen`.  For example,
  configuration like the following instruct `hs-bindgen` to use a version of GHC
  installed by GHCup.

    ```powershell
    $env:LLVM_PATH = "C:\ghcup\ghc\<your-ghc-version>\mingw"
    $env:LLVM_CONFIG = "$env:LLVM_PATH\bin\llvm-config.exe"
    $env:LIBCLANG_PATH = "$env:LLVM_PATH\lib"
    ```

* `BINDGEN_EXTRA_CLANG_ARGS`: This environment variable works the same on
  Windows as it does on Linux.

### DLLs

Windows shared libraries are called "dynamic-link libraries" (DLLs).  All DLLs
needed by an executable as well as any libraries it loads must be loaded when
the executable is run.  Unlike Linux and macOS, Windows uses the `PATH`
environment variable to search for DLLs (in addition to searching for
executables).

When building `hs-bindgen`, the `libclang-bindings` library is built.  It is
linked to `libclang` and `LLVM` DLLs as determined using the environment
variables described above.  When you run `hs-bindgen`, the `libclang.dll`
library is loaded by searching for that file in the directories listed in the
`PATH` environment variable.  The `PATH` environment variable should be
configured so that it loads the same installation of LLVM/Clang that
`hs-bindgen` and `libclang-bindings` was built with.  Note that the
`libclang.dll` filename is not versioned, and `hs-bindgen` can warn you when
the versions do not match.

```powershell
$env:Path = "$env:LLVM_PATH\bin;" + $env:Path
```

Similarly, when you create bindings that use a shared library, the directory
where that DLL can be found should be in your `PATH` environment variable at
runtime.

```powershell
$env:Path = "C:\path\to\your\c\libs;" + $env:Path
```

### Common Errors and Solutions

* LLVM/Clang version mismatch: If the version of the LLVM/Clang DLL that is
  loaded at runtime does not match the version used to build `hs-bindgen` and
  `libclang-bindings`, a warning it emitted.  The version does not change when
  using the installation of LLVM/Clang that is distributed with GHC, so this
  generally indicates that your `PATH` environment variable is incorrect.  Be
  sure that the `bin` directory for that LLVM/Clang installation is listed
  before any other LLVM/Clang installation directories.

  Use the `ldd` in a MinGW shell to debug DLL loading.

* DLL loading failure: If a linked library is not found, Windows applications
  generally fail silently with a negative exit code.  Use `ldd` in a MinGW shell
  to determine if a DLL is not found.  Resolve such issues by adding directories
  to your `PATH` environment variable.

  In GitHub CI, be sure to always use Windows-style paths (such as
  `C:\ghcup\ghc\9.14.1\mingw\bin`), *not* POSIX-style paths (such as
  `/c/ghcup/ghc/9.14.1/mingw/bin`), in the `PATH` environment variable.

* Unicode issues: The Windows assembler does not support Unicode, so avoid using
  Unicode-specific characters in C function definitions.

* Resolving issues with underlying type mismatches (`FC.CInt` vs.
  `FC.CUInt`): You might encounter Haskell type errors where, for example, a C
  `int` is being interpreted as a `CUInt` instead of a `CInt`. This is often
  due to how different compilers and platforms define basic types. Carefully
  check your C and Haskell type definitions to ensure they match.

## Nix

We provide and maintain a [Nix Flake](../flake.nix), and a [Nix-focused
tutorial](https://github.com/well-typed/hs-bindgen-tutorial-nix).
