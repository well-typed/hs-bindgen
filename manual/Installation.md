# Installation

Depending on the system, specific steps ensure a smooth experience with
`hs-bindgen`. This guide will walk you through the setup process for Linux,
macOS, and Windows.

## Linux

Setup on Linux involves choosing between the GCC and Clang compiler toolchains.

### Compiler choice (GCC vs. Clang)

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
    > Without `-no-stdinc`, there are likely multiple directories in the C
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

We provide and maintain a [Nix Flake](../../../flake.nix), and a [Nix-focused
tutorial](https://github.com/well-typed/hs-bindgen-tutorial-nix).
