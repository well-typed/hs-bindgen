# Cross-Compilation Example

This example demonstrates:

- Generating Haskell bindings for a different target architecture using
  `--clang-option="--target=<triple>"`
- Building Haskell executables with a cross-compiling GHC from Nix
- Running cross-compiled binaries under QEMU user-mode emulation

## Quick start

```bash
cd examples/cross-compilation
nix develop
./generate-and-run.sh           # all targets
./generate-and-run.sh aarch64   # 64-bit ARM only
./generate-and-run.sh native    # native only
```

## Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled

Enable flakes by adding to `~/.config/nix/nix.conf`:

```
experimental-features = nix-command flakes
```

## Directory structure

```
cross-compilation/
├── flake.nix              # Nix environment (cross-GHC, QEMU, sysroots)
├── generate-and-run.sh    # Full workflow: build, generate, cross-compile, run
├── compare-sizes.sh       # Compare generated struct sizes across targets
├── c-src/
│   ├── arch_types.h       # Header with architecture-dependent types
│   ├── arch_types.c       # Implementation
│   └── Makefile
└── hs-project/
    ├── cross-compilation-example.cabal
    ├── cabal.project
    ├── app/Main.hs        # Executable that prints struct sizes
    ├── src-native/        # Generated bindings for native platform
    └── src-aarch64/       # Generated bindings for 64-bit ARM
```

## What the script does

The `generate-and-run.sh` script runs a four-phase pipeline:

1. **Build C libraries** - compiles `arch_types.c` for native and aarch64
   using native gcc and cross-compilers from Nix

2. **Generate Haskell bindings** - runs `hs-bindgen-cli preprocess` for each
   target. The key option is `--clang-option="--target=<triple>"`, which tells
   libclang to use the target's type sizes, pointer sizes, and alignment rules

3. **Build and run native** - builds and runs the Haskell executable with the
   native GHC to verify it works

4. **Cross-compile and run under QEMU** - builds the Haskell executable with
   a cross-compiling GHC from Nix, using iserv (external interpreter) for
   Template Haskell splices, then runs the result under QEMU

## The key hs-bindgen option

```bash
hs-bindgen-cli preprocess \
  --clang-option="--target=aarch64-linux-gnu" \
  -I c-src \
  --module ArchTypes.Generated \
  --hs-output-dir hs-project/src-aarch64 \
  c-src/arch_types.h
```

The `--target` flag makes libclang use the target's data model. For example,
`struct ArchInfo` is 24 bytes on LP64 (64-bit) but 16 bytes on ILP32 (32-bit)
because `long` and pointer sizes differ.

## Comparing struct sizes

After running `generate-and-run.sh` you should already be able to compare the
different sizes, however to compare the generated sizes after binding
generation:

```bash
./compare-sizes.sh
```

This extracts `sizeOf`, `alignment`, and field offsets from the generated
Haskell `Storable` instances and displays them side by side.

## Known issues and workarounds

### LLVM ARM32 crash (GHC 9.10+)

GHC 9.10 and later cannot cross-compile to ARM32 because of an LLVM bug:
`llc` segfaults in `ARMAsmPrinter::emitXXStructor` with a null `Subtarget`
pointer. This affects **all LLVM versions 17 through 21**. This is why we
pin to `nixos-25.05` (GHC 9.8.4 / LLVM 15) — see `flake.nix` for details.

### GHC ARM Thumb interworking bug

ARM32 cross-compilation with Template Haskell is blocked by a GHC runtime
linker bug that causes `SIGILL`. See
[GHC #26937](https://gitlab.haskell.org/ghc/ghc/-/issues/26937) for details
and a reproducer.

## Further reading

- [hs-bindgen Cross-Compilation Manual](../../manual/LowLevel/Usage/08-CrossCompilation.md) --
  authoritative reference covering target triples, sysroots, iserv/TH
  cross-compilation, and troubleshooting
- [Clang Cross-Compilation](https://clang.llvm.org/docs/CrossCompilation.html)
- [QEMU User Mode Emulation](https://www.qemu.org/docs/master/user/main.html)
- [Nix Cross-Compilation](https://nixos.wiki/wiki/Cross_Compiling)
- [GHC External Interpreter (iserv)](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/external-interpreter)
