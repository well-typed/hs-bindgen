# Troubleshooting Guide

This document is a collection of trouleshooting the developers gathered a long
the time, they provide solutions to common issues encountered when developing
with `hs-bindgen`.

## Build Issues

### Missing C Libraries (Example from the manual)

Error:

```text
Error: [Cabal-4345]
Missing dependency on a foreign library:
* Missing (or bad) C library: game
* Missing (or bad) C library: vector
```

Solution:

1. Build the C libraries first:

   ```bash
   cd manual/c
   make
   cd ..
   ```

2. Create a `cabal.project.local` file with proper paths:

   ```cabal
   package hs-game
     extra-include-dirs: /path/to/hs-bindgen/manual/c
     extra-lib-dirs:     /path/to/hs-bindgen/manual/c

   package hs-vector
     extra-include-dirs: /path/to/hs-bindgen/manual/c
     extra-lib-dirs:     /path/to/hs-bindgen/manual/c

   package manual
     extra-include-dirs: /path/to/hs-bindgen/manual/c
     extra-lib-dirs:     /path/to/hs-bindgen/manual/c
   ```

### Missing Header Files (Example from the manual)

Error:

```text
fatal error: manual_examples.h: No such file or directory
#include "manual_examples.h"
```

Solution:

- Verify header file paths in your `cabal.project.local` configuration
- Ensure `extra-include-dirs` points to the correct directory
- Check that header files exist in the specified location

### Linking Issues (Bindings for c-example)

Error:

```text
undefined reference to 'hs_bindgen_c_example_helloworld'
```

Solution:
Add the C library to your `.cabal` file:

```cabal
extra-libraries: hs-bindgen-c-example
```

Note: Use the library name (without extension), this will make cabal/ghc look
for `lib<name>.so` binary file name.

## Runtime Issues

### Dynamic Library Loading

#### Shared Library Not Found (Bindings for c-example)

Error:

```text
error while loading shared libraries: libhs-bindgen-c-example.so: cannot open shared object file: No such file or directory
```

Solution:

Set `LD_LIBRARY_PATH`, on Linux, to include your C library directory:

```bash
export LD_LIBRARY_PATH=/path/to/your/c/libs:$LD_LIBRARY_PATH
```

Use `DYLD_LIBRARY_PATH` instead of `LD_LIBRARY_PATH`, on MacOS:

```bash
export DYLD_LIBRARY_PATH=/path/to/your/c/libs:$DYLD_LIBRARY_PATH
```

Use `PATH` instead of `LD_LIBRARY_PATH`, on Windows:

```powershell
$env:PATH = "C:\path\to\your\c\libs;" + $env:PATH
```

## Platform-Specific Issues

### Unicode Character Issues (Building and running the manual)

Solution:

- Linux: Usually works
- macOS/Windows/LLVM: Avoid Unicode characters in C code
- Use CPP macros to conditionally handle Unicode:

  ```c
  #if defined(SUPPORTS_UNICODE)
  #else
  #endif
  ```

### Windows-Specific Issues

#### Type Differences (Building and running the manual)

Issue: Generated code uses `FC.CInt` instead of `FC.CUInt`

Solution:

Add CPP conditionals to handle Windows type differences:

```haskell
#if defined(mingw32_HOST_OS)
type Index = FC.CUInt
#else
type Index = FC.CInt
#endif
```
