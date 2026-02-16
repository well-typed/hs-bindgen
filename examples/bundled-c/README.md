# Bundled C example

This example demonstrates using `hs-bindgen` with C source files compiled
directly into the Haskell package via `c-sources`, instead of linking against a
pre-built shared library.

## How it works

The `.cabal` file uses `c-sources` and `include-dirs` instead of
`extra-libraries`:

```cabal
executable bundled-c-bin
  c-sources:    cbits/rect.c
  include-dirs: cbits
```

Cabal compiles `rect.c` and links it into the executable automatically.

**Note:** GHC compiles `c-sources` with its configured C compiler (typically GCC
on Linux), while `hs-bindgen` uses `libclang` to parse headers and derive type
layouts. For simple types this is unlikely to cause problems, but GCC and Clang
can disagree on memory layout for more exotic constructs (bitfields, packed
structs, platform-specific alignment). See the [Compiler choice (GCC
vs. Clang)](../../../manual/LowLevel/Usage/01-Invocation.md#compiler-choice-gcc-vs-clang)
section in the manual for details.

## Prerequisites

No additional prerequisites beyond the standard `hs-bindgen` setup (GHC, Cabal,
LLVM/Clang). No external C libraries need to be installed.

## Running

```bash
cd examples/bundled-c
./generate-and-run.sh
```

The script runs `hs-bindgen-cli` on the header file to generate Haskell
bindings, then builds and runs the executable.

## CPP flag mismatch demo

This example also demonstrates the non-portability problem described in
[issue #893](https://github.com/well-typed/hs-bindgen/issues/893) and the
[Non-portability](../../../manual/LowLevel/Usage/06-BindingSpecifications.md#non-portability)
section of the manual.

### Setup

The header `rect.h` contains a conditional field controlled by `RECT_3D`:

```c
typedef struct {
    double x;
    double y;
#ifdef RECT_3D
    double z;
#endif
    double width;
    double height;
} rect;
```

Without `RECT_3D`, the struct has 4 fields (32 bytes):
`x@0, y@8, width@16, height@24`.

With `RECT_3D`, the struct has 5 fields (40 bytes):
`x@0, y@8, z@16, width@24, height@32`.

The `.cabal` file provides a `rect-3d` flag that passes `-DRECT_3D` to the C
compiler (via `cc-options`). This flag does *not* affect `hs-bindgen`; to pass
`-DRECT_3D` to `hs-bindgen`, use `--clang-option="-DRECT_3D"` when generating
bindings.

`Main.hs` constructs a `Rect` with `width = 5.0` and `height = 3.0`, calls the
C functions `rect_area` and `rect_perimeter`, and asserts the results equal
`15.0` and `16.0` respectively.

### All four flag combinations

The key question is whether `hs-bindgen` (which determines the Haskell
`Storable` field offsets) and the C compiler (which determines the C struct
layout) agree on the struct definition.

| `hs-bindgen` | C compiler (`cabal build`) | Agreement? | Outcome |
|---|---|---|---|
| no flag | no flag | Yes (4-field) | Correct |
| `-DRECT_3D` | `-DRECT_3D` | Yes (5-field) | Correct |
| `-DRECT_3D` | no flag | No | **Runtime failure**: Haskell writes 5-field layout, C reads 4-field layout |
| no flag | `-DRECT_3D` | No | **Runtime failure**: Haskell writes 4-field layout, C reads 5-field layout |

The last two rows are the mismatched cases. Both produce runtime data corruption:

- **`hs-bindgen` with flag, C without**: `hs-bindgen`
  generates a 5-field `Storable` (40 bytes). Haskell writes `width` at offset 24
  and `height` at offset 32. The C function reads `width` from offset 16 (which
  contains `z`) and `height` from offset 24 (which contains `width`). Area =
  `z * width` instead of `width * height`.

- **`hs-bindgen` without flag, C with**: `hs-bindgen`
  generates a 4-field `Storable` (32 bytes). Haskell writes `width` at offset 16
  and `height` at offset 24. The C function reads `width` from offset 24 (which
  contains `height`, 3.0) and `height` from offset 32 (past the allocation,
  stack junk). Area = `3.0 * junk` instead of `width * height`.

The `test-cpp-mismatch.sh` script tests the second mismatched case because it
does not require changes to `Main.hs` (the generated `Rect` still has 4
fields).

### Running the mismatch test

```bash
cd examples/bundled-c
./test-cpp-mismatch.sh
```

The script generates bindings without `-DRECT_3D`, then builds with
`cabal build -frect-3d`, and runs the program. It expects the assertions to
fail (exit code ≠ 0) and reports success when they do.
