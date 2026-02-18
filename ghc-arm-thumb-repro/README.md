# GHC ARM Thumb interworking bug — minimal reproducer

Minimal reproducer for a bug in GHC's runtime linker on ARM32 where Template
Haskell's `addForeignSource` produces unlinked `.o` files with `STT_NOTYPE`
symbols, causing the runtime linker to generate wrong branch instructions
(BL instead of BLX) for Thumb targets.

**Result:** `SIGILL` (Illegal Instruction) during TH evaluation when calling
Thumb-compiled functions in shared libraries (e.g., glibc's `strlen`).

## How to reproduce

```bash
cd ghc-arm-thumb-repro
nix develop          # provides cross-GHC 9.8.4 for ARM32, QEMU, cross-GCC
./reproduce.sh       # crashes with SIGILL
```

Requires an x86_64 Linux host with Nix (flakes enabled). The first `nix develop`
invocation builds the cross-GHC from source, which takes a while.

## What happens

1. `Lib.hs` uses TH `addForeignSource` to emit C code that calls `strlen`
2. GHC compiles the C code with `gcc -c` (compile only, **no link step**)
3. The resulting `.o` file has `strlen` as `STT_NOTYPE, UND` (no type info)
4. GHC sends the `.o` to iserv (running under QEMU) for TH evaluation
5. The runtime linker resolves `strlen` via `dlsym` — gets address `0x...341` (bit 0 = 1 = Thumb)
6. But `rts/linker/Elf.c` only checks bit 0 for `STT_FUNC` symbols, and the `.o` has `STT_NOTYPE`
7. The linker generates ARM `BL` instead of `BLX` — CPU executes Thumb code in ARM mode — **SIGILL**

## Root cause

The C source from `addForeignSource` is compiled with `gcc -c` but never
linked (`GHC/Driver/Pipeline/Execute.hs:502`). In a normal build, the static
linker (`ld`) reads `libc.so`'s symbol table, sees `STT_FUNC`, and handles
Thumb interworking. With TH/iserv, the `.o` goes directly to the runtime
linker, which only has the unlinked `.o`'s `STT_NOTYPE`.

## Environment

- GHC 9.8.4 cross-compiling to `armv7l-unknown-linux-gnueabihf`
- nixos-25.05 (pinned for GHC 9.8 — GHC 9.10+ hits a separate LLVM bug)
- QEMU user-mode emulation
- glibc 2.40 (ARM32)

## Related

- GHC #21991 — Many failures on ARMv7 in ghci-ext way
- GHC #10969 — Arm: Investigate Thumb2/Arm interworking
- [AAELF32 spec](https://github.com/ARM-software/abi-aa/blob/main/aaelf32/aaelf32.rst) §4.5.3, §5.5.2
