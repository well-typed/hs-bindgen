# Literate Example

This example demonstrates how to integrate `hs-bindgen` with Cabal's build system using the literate Haskell preprocessor mechanism.

This approach uses Cabal's support for custom literate Haskell preprocessors to trigger binding generation during compilation, without requiring external build systems or custom setup scripts.

## Background

Binding generation requires running `hs-bindgen` before GHC compiles Haskell code.  Several approaches exist to orchestrate this:

1. **External build system** - Use Make, Nix, or similar tools to run `hs-bindgen-cli preprocess` before Cabal
2. **Custom setup script** - Write a `Setup.hs` that invokes `hs-bindgen-cli` (discouraged; poor tooling integration, particularly with HLS)
3. **Cabal hooks** - Use Cabal's hooks infrastructure (requires very recent Cabal versions; not yet fully explored)
4. **Literate preprocessor** - Configure `.lhs` files to use `hs-bindgen-cli` as the preprocessor (this approach)

The literate preprocessor approach (option 4) leverages Cabal's support for literate Haskell.  Haskell modules can have the `.lhs` extension to mark them as literate Haskell.  When compiling such files, Cabal runs them through a preprocessor (normally `unlit`) to generate the `.hs` file before GHC compilation.  The preprocessor can be changed using the `-pgmL` GHC flag.

By configuring `hs-bindgen-cli` as the preprocessor, binding generation occurs automatically during `cabal build`.  Instead of literate Haskell markup, the `.lhs` file contains configuration flags for `hs-bindgen` in the form of a Haskell list.

The Template Haskell mode avoids the need for any of these approaches, but is less suitable for cross-compilation scenarios where target platform information may not be available at compile time.

## Structure

```
literate-example/
├── c-lib/
│   ├── simple.h        -- C header with declarations
│   ├── simple.c        -- C implementation
│   └── Makefile        -- Build script for shared library
├── app/
│   ├── Main.hs             -- Example executable
│   └── SimpleBindings.lhs  -- Configuration file (Haskell list of arguments)
└── literate-example.cabal
```

## How it works

1. The `.cabal` file specifies `hs-bindgen-cli` as the literate Haskell preprocessor:
   ```cabal
   executable literate-example
     ghc-options:
         -pgmL hs-bindgen-cli
         -optL tool-support
         -optL literate
     build-tool-depends:
         hs-bindgen:hs-bindgen-cli
   ```

   - `-pgmL hs-bindgen-cli` specifies `hs-bindgen-cli` as the literate preprocessor
   - `-optL tool-support -optL literate` passes arguments to the preprocessor
   - `build-tool-depends` ensures `hs-bindgen-cli` is built and available

2. The `SimpleBindings.lhs` file contains a Haskell list of command-line arguments:
   ```haskell
   [ "-I", "./c-lib"
   , "--module=SimpleBindings"
   , "--unique-id", "org.hs-bindgen.literate-example"
   , "--enable-program-slicing"
   , "simple.h"
   ]
   ```

   These are the same arguments you would pass to `hs-bindgen-cli preprocess`.

   **The file can contain anything** — it's just passed to the preprocessor, which reads it and generates Haskell code.

3. When `cabal build` is invoked:
   - Cabal detects the `.lhs` file
   - Invokes `hs-bindgen-cli tool-support literate app/SimpleBindings.lhs app/SimpleBindings.hs -I c-lib --module=SimpleBindings --unique-id org.hs-bindgen.literate-example --enable-program-slicing simple.h`
   - The preprocessor reads the configuration from the file
   - The preprocessor generates bindings (equivalent to `hs-bindgen-cli preprocess`)
   - The preprocessor writes generated code to the output path
   - GHC compiles the resulting `.hs` file

## Building

First, build the C library:

```bash
cd c-lib
make
cd ..
```

This creates the shared library (`libc-lib.so` on Linux, `libc-lib.dylib` on macOS, or `c-lib.dll` on Windows).

Then build the Haskell project:

```bash
cabal build
```

Note: You may need to set `LD_LIBRARY_PATH` (Linux), `DYLD_LIBRARY_PATH` (macOS), or `PATH` (Windows) to include the `c-lib` directory for the runtime linker to find the library.

## Running

```bash
LD_LIBRARY_PATH=c-lib cabal run literate-example
```

## Notes

- The bindings are regenerated automatically when the `.lhs` file or C headers change
- No separate build script is needed
- No custom preprocessor executable needed
- Configuration lives with the Haskell source code
- The `.lhs` file content is arbitrary—it's just passed to the preprocessor
- This approach is used in the hs-bindgen test suite (see `hs-bindgen/test/pp`)
