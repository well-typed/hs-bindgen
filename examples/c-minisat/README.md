# MiniSat Haskell Bindings

This example demonstrates generating Haskell bindings for
[MiniSat](https://github.com/niklasso/minisat-c-bindings), a minimalistic
open-source SAT solver.

## Prerequisites

You need the MiniSat library installed on your system. For example, on NixOS:

```bash
nix-shell -p minisat
```

## Running the Example

```bash
./generate-and-run.sh
```

This script will:
1. Build the minisat-c-bindings C library
2. Create symbolic links for the shared library (see quirks below)
3. Generate Haskell bindings using `hs-bindgen`
4. Create `cabal.project.local` with the necessary configuration
5. Build and run the example program

## Quirks and Workarounds

### 1. Shared Library Symlinks

The build process creates `libminisat-c.so.1.0.0`, but the linker needs the
standard naming convention symlinks:

```bash
ln -sf libminisat-c.so.1.0.0 libminisat-c.so
ln -sf libminisat-c.so.1.0.0 libminisat-c.so.1
```

The `generate-and-run.sh` script handles this automatically. These symlinks
follow Linux shared library naming conventions:

- `libminisat-c.so` → linker name (used at compile time)
- `libminisat-c.so.1` → soname (used at runtime)
- `libminisat-c.so.1.0.0` → real name (actual library file)

See [this explanation](https://stackoverflow.com/questions/663209/can-someone-explain-linux-library-naming/21462448#21462448)
for more details.

### 2. Linking Against the C Library

The `.cabal` file includes `extra-libraries: minisat-c`, which tells the
linker to link against `libminisat-c.so`. The library name should match the
base name of the binary file (without the `lib` prefix and `.so` extension).
