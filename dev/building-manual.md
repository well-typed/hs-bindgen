# Building the Manual

This guide provides step-by-step instructions for building and running the
`hs-bindgen` manual on different platforms. The manual serves as both
documentation and a comprehensive test of `hs-bindgen`'s capabilities.

## Prerequisites

Before building the manual, ensure you have completed the environment setup
for your platform as described in [dev-environment.md](dev-environment.md).

## Standard Build

On the [`manual`](../manual) directory, create the following
`cabal.project.local` file:

```
package manual
  extra-include-dirs:
      /path/to/hs-bindgen/manual/c
  extra-lib-dirs:
      /path/to/hs-bindgen/manual/c

package hs-game
  extra-include-dirs:
      /path/to/hs-bindgen/manual/c
  extra-lib-dirs:
      /path/to/hs-bindgen/manual/c

package hs-vector
  extra-include-dirs:
      /path/to/hs-bindgen/manual/c
  extra-lib-dirs:
      /path/to/hs-bindgen/manual/c
```

On Linux, since it supports Unicode characters, also add the following to the
file:

```
package *
  ghc-options:
    -optc-DSUPPORTS_UNICODE
    -DSUPPORTS_UNICODE
```

Change directory to the [`c/`](../manual/c/) folder and compile the binaries
using the Makefile.

```bash
cd c/
make
cd ..
```

Generate bindings using the [generate.sh](../manual/generate.sh) script:

```bash
./generate.sh
```

Finally, run the manual:

```bash
cd hs/
cp ../cabal.project.local .
cabal run manual
```

## Platform-Specific Notes

### Unicode Support

- Linux: Full Unicode support available with `SUPPORTS_UNICODE=1`
- macOS: No Unicode support (Apple assembler limitation), disabled by default.
- Windows: No Unicode support (Windows assembler limitation), disabled by default.
- LLVM Backend: No Unicode support (LLVM IR limitation), disabled by default.

### Common Issues

- Missing headers: If you encounter errors about missing standard headers
  like `stddef.h`, it indicates that `libclang` cannot find the system's include
  directories. To fix this please do the following:

  ```bash
  # Get system include paths
  CLANG_ARGS=$(clang -v -E -xc /dev/null 2>&1 | awk '/#include <...> search starts here/{f=1;next}/End of search list./{f=0}f{sub(/^[ \t]*/,"",$0);s=s " -I"$0}END{print substr(s,2)}')

  # Set BINDGEN_EXTRA_CLANG_ARGS with -nostdinc to avoid duplicate headers
  export BINDGEN_EXTRA_CLANG_ARGS="-nostdinc $CLANG_ARGS"
  ```

- Missing shared libraries: If you see `cannot open shared object file: No such file or directory`,
  add the library's directory to `LD_LIBRARY_PATH` on Linux,
  `DYLD_LIBRARY_PATH` on MacOS and `PATH` on Windows.

- Unicode-related compilation errors: Verify that `SUPPORTS_UNICODE` is only
  set on platforms that support it (Linux with standard backend).

- Path separators: On Windows, if not on a `mingw` environment, use
  backslashes in `cabal.project.local` paths,
  but forward slashes work fine in shell commands.

- SDK headers: macOS requires the `SDKROOT` environment variable to locate
  system headers properly.

- Silent failures: On Windows, if your application fails with cryptic error
  codes like `ExitFailure (-1073741515)`, it's usually a DLL loading issue.
  Ensure your C library DLLs are in `PATH`.

### More troubleshooting

See [here](./troubleshooting.md)
