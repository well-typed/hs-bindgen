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
./generate-and-run.sh
```

## Prerequisites

- [Nix](https://nixos.org/download/) with flakes enabled

Enable flakes by adding to `~/.config/nix/nix.conf`:

```
experimental-features = nix-command flakes
```

## Directory structure

```
cross-compilation/
├── flake.nix              # Nix environment (cross-GHC, QEMU, sysroots)
├── generate-and-run.sh    # Full workflow: build, generate, cross-compile, run
├── c-src/
│   ├── arch_types.h       # Header with architecture-dependent types
│   ├── arch_types.c       # Implementation
│   └── Makefile
└── hs-project/
    ├── cross-compilation-example.cabal
    ├── cabal.project
    ├── app/Main.hs        # Preprocess-mode executable (uses pre-generated
    │                      # bindings from src-aarch64/)
    ├── app/MainTH.hs      # TH-mode executable (generates bindings inline
    │                      # via withHsBindgen splice)
    └── src-aarch64/       # Generated bindings for 64-bit ARM
```

## What the script does

`generate-and-run.sh` runs a four-phase pipeline:

1. **Build C library** -- cross-compile `arch_types.c` for aarch64 using
   Nix's cross-compiling Clang wrapper.

2. **Generate Haskell bindings (preprocess mode)** -- run
   `hs-bindgen-cli preprocess --clang-option="--target=aarch64-linux-gnu"`
   to write `.hs` files with aarch64 type sizes and alignments.

3. **Cross-compile and run preprocess-mode executable** -- the cross-GHC
   compiles the generated bindings, iserv handles internal TH splices,
   QEMU runs the result.

4. **Cross-compile and run TH-mode executable** -- a second executable
   that calls `withHsBindgen ... $ hashInclude "..."` directly. The
   splice runs inside iserv-on-QEMU and dlopens a target-arch
   `libclang.so` to parse the header at compile time. Heavier than
   Phase 3 because the full `hs-bindgen` package and its libclang/zlib
   transitive deps must be cross-built.

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

## TH-mode cross-compilation

Phase 4 of `generate-and-run.sh` builds a second executable
(`cross-compilation-aarch64-th`) that uses hs-bindgen in Template Haskell
mode. Compared to preprocess mode, TH mode has additional requirements:

- **The full `hs-bindgen` package is cross-built**, not just
  `hs-bindgen-runtime`. This is significantly heavier the first time.
- **Target-architecture `libclang.so` must be reachable from QEMU.** The TH
  splice runs inside iserv (a target-arch binary under QEMU), so when the
  splice dlopens libclang, that libclang must be aarch64. The flake exposes
  this as `AARCH64_LIBCLANG_LIB`, and the iserv wrapper script sets
  `LD_LIBRARY_PATH` accordingly.
- **Two transitive cross-deps need scoped workarounds**, both applied just
  for Phase 4:
  - `zlib`: pkg-config in this cross setup is not target-aware; add the target
    zlib's include dir to `package *` so the cross-CC finds the header. (The
    `+bundled-c-zlib` flag is an alternative if you don't have a target
    zlib's headers handy.)
  - `libclang-bindings`: point its `configure.ac` at a tiny
    `llvm-config` stub script that returns target-arch paths via
    `LLVM_CONFIG=...`.

See [`manual/low-level/usage/cross-compilation.md`](../../manual/low-level/usage/cross-compilation.md)
for the full TH-mode workflow.

## Drawbacks of the TH API for cross-compilation

This example demonstrates how cross-compilation of a package that uses the
`hs-bindgen` Template Haskell API can be done, but note that there are
drawbacks to doing this.

A library/executable that uses the `hs-bindgen` Template Haskell API must
have `hs-bindgen` as a dependency (in addition to `hs-bindgen-runtime`).
The built library/executable is therefore linked to `libclang`, and the
same major/minor version of LLVM/Clang must be installed on any system the
library/executable is used.  One generally does not want compile-time
dependencies to be dependencies of the built library/executable, but this
is a cost of using the Template Haskell API.  The situation might be
improved/resolved in the future with the introduction of explicit level
imports, *if* system dependencies of compile time dependencies are not
linked in the built libraries/executables.

This cost is likely much more significant in situations when
cross-compilation is required.  When cross-compiling, you likely want to
use the CLI, so that `hs-bindgen` is not a dependency of your
library/executable and `libclang` is not linked.

## Known issues and workarounds

### LLVM ARM32 crash (GHC 9.10+)

GHC 9.10 and later cannot cross-compile **to ARM32** because of an LLVM
bug: `llc` segfaults in `ARMAsmPrinter::emitXXStructor` with a null
`Subtarget` pointer. This affects all LLVM versions 17 through 21. The
bug is in the 32-bit ARM backend specifically; aarch64 uses
`AArch64AsmPrinter` and is not affected. ARM32 has been removed from
this example, so the `nixos-25.05` pin in `flake.nix` is conservative
for aarch64-only and could likely be bumped without re-introducing this
crash.

### GHC ARM Thumb interworking bug

ARM32 cross-compilation with Template Haskell is blocked by a GHC runtime
linker bug that causes `SIGILL`. See
[GHC #26937](https://gitlab.haskell.org/ghc/ghc/-/issues/26937) for details
and a reproducer.

## Further reading

- [hs-bindgen Cross-Compilation Manual](../../manual/low-level/usage/cross-compilation.md) --
  authoritative reference covering target triples, sysroots, iserv/TH
  cross-compilation, and troubleshooting
- [Clang Cross-Compilation](https://clang.llvm.org/docs/CrossCompilation.html)
- [QEMU User Mode Emulation](https://www.qemu.org/docs/master/user/main.html)
- [Nix Cross-Compilation](https://wiki.nixos.org/wiki/Cross_Compiling)
- [GHC External Interpreter (iserv)](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/external-interpreter)
