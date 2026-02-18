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

Setup on Windows involves handling of Dynamic-Link Libraries (DLLs) and the
configuration of the compiler environment.

### Built-in Clang with GHC installation

When you install GHC on Windows using GHCup, for example, it comes with a MinGW
environment that includes LLVM and Clang. Windows' assembler does not support
Unicode, so avoid using Unicode-specific characters in C function definitions.

### Explicit handling of `PATH` for DLLs

At runtime, Windows primarily finds DLLs by searching the directories listed in
the `PATH` environment variable. This is a crucial difference from Linux and
MacOS.

* To ensure your application can find the required DLLs, add the corresponding
  directories to the `PATH`:

  ```powershell
  $env:PATH = "C:\path\to\your\c\libs;" + $env:PATH
  ```

### Environment Variables

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

### Common Errors and Solutions

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

  ```cabal
  if !(os(linux) && arch(x86_64))
    buildable: false
  ```

## Nix

We provide and maintain a [Nix Flake](../../../flake.nix), and a [Nix-focused
tutorial](https://github.com/well-typed/hs-bindgen-tutorial-nix).
