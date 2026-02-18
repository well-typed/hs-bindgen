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
./generate-and-run.sh arm32     # 32-bit ARM only
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
    ├── src-aarch64/       # Generated bindings for 64-bit ARM
    └── src-arm32/         # Generated bindings for 32-bit ARM
```

## What the script does

The `generate-and-run.sh` script runs a four-phase pipeline:

1. **Build C libraries** - compiles `arch_types.c` for native, aarch64, and
   arm32 using native gcc and cross-compilers from Nix

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
pointer. This affects **all LLVM versions 17 through 21**.

GHC 9.10+ generates LLVM IR with `@llvm.global_ctors` entries that have no
function body in the same module. LLVM's ARM backend tries to access
subtarget information for these entries during assembly emission, but the
subtarget is null because no function was processed. GHC 9.8 generates
different IR that does not trigger this code path.

The fix is merged to LLVM `main` (PR
[#166329](https://github.com/llvm/llvm-project/pull/166329)), but the
backport to `release/21.x` was rejected
([#168380](https://github.com/llvm/llvm-project/pull/168380)). Until LLVM 22
ships (which will include the fix), we pin to `nixos-25.05` (GHC 9.8.4 /
LLVM 15).

**Upstream issues:**
- [llvm/llvm-project#165422](https://github.com/llvm/llvm-project/issues/165422)
- [ghc#26510](https://gitlab.haskell.org/ghc/ghc/-/issues/26510)
- [NixOS/nixpkgs#466116](https://github.com/NixOS/nixpkgs/issues/466116)

### GHC ARM Thumb interworking bug (`ghc-arm-thumb-interworking.patch`)

When cross-compiling Haskell code with Template Haskell for ARM32, GHC's
runtime linker crashes with `SIGILL` (Illegal instruction). This section
explains what the bug is, why it happens, and what the patch does.

#### Background: ARM vs Thumb instruction sets

ARM32 processors support two instruction sets:

- **ARM mode**: all instructions are 32 bits wide
- **Thumb mode** (specifically Thumb-2): instructions are a mix of 16-bit and
  32-bit, more compact

A single ARM32 binary can contain functions in both modes. The CPU tracks
which mode it's in and must be switched explicitly. When calling a function,
the instruction used matters:

| Instruction | What it does                                    |
|-------------|------------------------------------------------|
| `BL`        | Branch-and-link (call). **Stays in same mode** |
| `BLX`       | Branch-and-link with exchange. **Switches mode** (ARM↔Thumb) |

If ARM-mode code uses `BL` to call a Thumb function, the CPU stays in ARM
mode and tries to decode Thumb instructions as ARM instructions →
**`SIGILL`** (illegal instruction).

#### How the CPU knows which mode a function uses

In ARM32 ELF binaries, function addresses encode the mode in **bit 0**:

- Bit 0 = 0 → ARM function
- Bit 0 = 1 → Thumb function

This is called the **Thumb bit**. The CPU ignores bit 0 for alignment
purposes (instructions are at least 2-byte aligned) and uses it purely as a
mode indicator. When you call `dlsym(RTLD_DEFAULT, "strlen")` on ARM32
Linux, the returned pointer has bit 0 set if `strlen` is compiled as Thumb
code (which it is in glibc).

#### What happens during Template Haskell cross-compilation

1. GHC is cross-compiling Haskell code for ARM32 on an x86_64 host
2. The code uses Template Haskell (TH), which must execute at compile time
3. GHC spawns **iserv** (external interpreter) — an ARM32 binary running
   under QEMU
4. When TH splices use `addForeignSource` (or anything that triggers file
   I/O), GHC's **runtime linker** inside iserv loads ARM32 object files
5. These object files have **relocations** — instructions that say "call
   function X" where X hasn't been resolved to an address yet
6. The runtime linker resolves X (e.g., `strlen`) using `dlsym` and patches
   the call instruction

#### The bug (GHC [#21991](https://gitlab.haskell.org/ghc/ghc/-/issues/21991))

In `rts/linker/Elf.c`, when the runtime linker resolves a symbol and
needs to generate a branch instruction, it checks whether the target is a
Thumb function so it can use `BLX` instead of `BL`. **But it only checks
the Thumb bit for symbols with ELF type `STT_FUNC`**:

```c
// rts/linker/Elf.c, around line 1262
if(ELF_ST_TYPE(symbol->elf_sym->st_info) == STT_FUNC) {
    is_target_thm = S & 0x1;  // Check Thumb bit
    T = is_target_thm;
    S &= ~1;                  // Clear Thumb bit from address
}
```

The problem: when iserv loads an object file that references `strlen`, the
ELF symbol table entry for `strlen` in that object file has type
**`STT_NOTYPE`** (because `strlen` is an imported/undefined symbol — it's
not defined in the object file). So the `STT_FUNC` check fails, the Thumb
bit check is skipped, `is_target_thm` stays 0, and the linker generates a
plain `BL` instruction.

When the CPU executes this `BL` to call glibc's `strlen` (which is Thumb-2
code), it stays in ARM mode. The first Thumb-2 instruction of `strlen`
(`pld [r0]`, encoded as `f890 f000`) gets decoded as an invalid ARM
instruction → **`SIGILL`**.

#### What the patch does

The patch adds a fallback check: if the ELF symbol type is NOT `STT_FUNC`,
check whether bit 0 of the resolved address is set. If it is, the target
is a Thumb function regardless of the ELF type:

```c
if(ELF_ST_TYPE(symbol->elf_sym->st_info) == STT_FUNC) {
    is_target_thm = S & 0x1;
    T = is_target_thm;
    S &= ~1;
} else if(S & 0x1) {
    // Symbol resolved via dlsym or with unknown ELF type
    // (e.g. STT_NOTYPE for imported/undefined symbols).
    // On ARM, dlsym preserves the Thumb bit (bit 0) for
    // Thumb functions.  Detect it from the address itself
    // so that we generate BLX (not BL) for ARM->Thumb calls.
    is_target_thm = 1;
    T = 1;
    S &= ~1;
}
```

This is safe because:
- On ARM32, `dlsym` preserves the Thumb bit — this is documented Linux/ARM
  behaviour
- No valid ARM code address has bit 0 set (instructions are at least
  2-byte aligned), so bit 0 = 1 always means "Thumb function"
- The existing `STT_FUNC` path is unchanged; this only adds handling for
  symbols where the ELF type wasn't informative

#### How we apply the patch

In `flake.nix`, we override the cross-compiled GHC's derivation to add
the patch:

```nix
patchedGhcArm32 = pkgsArm32.buildPackages.ghc.overrideAttrs (old: {
  patches = (old.patches or []) ++ [
    ./ghc-arm-thumb-interworking.patch
  ];
});
```

This rebuilds GHC from source with the fix applied to `rts/linker/Elf.c`.

#### Upstreaming to GHC

This patch should be submitted upstream to GHC. Here's how:

1. **Create a GitLab account** at https://gitlab.haskell.org/
2. **Fork GHC**: https://gitlab.haskell.org/ghc/ghc/-/forks/new
3. **Clone your fork** and create a branch:
   ```bash
   git clone https://gitlab.haskell.org/<you>/ghc.git
   cd ghc
   git checkout -b fix/arm-thumb-interworking-dlsym
   ```
4. **Apply the patch** to `rts/linker/Elf.c` (the patch context is for GHC
   9.8.4 but should apply cleanly to HEAD — the code hasn't changed)
5. **Add a Note** in `rts/linker/Elf.c` above the new code explaining the
   `STT_NOTYPE` / dlsym interaction (GHC convention is to document
   non-obvious runtime behaviour with `Note [...]` references)
6. **Create a merge request** referencing
   [#21991](https://gitlab.haskell.org/ghc/ghc/-/issues/21991)
7. **Add a test** if possible — the GHC test suite has `ghci` tests that
   exercise the runtime linker; a test that loads a symbol via dlsym on ARM
   would be ideal but may require ARM CI infrastructure

The existing upstream issue #21991 ("Many failures on ARMv7 in ghci-ext
way") tracks this class of bugs. The root cause was partially addressed in
2015 (commit `933adc0f`, forcing GHC-generated code to use ARM mode), but
that fix only prevents GHC's *own* code from being Thumb — it doesn't help
when the runtime linker calls *system libraries* (like glibc) that are
compiled as Thumb.

## Further reading

- [hs-bindgen Cross-Compilation Manual](../../manual/LowLevel/Usage/08-CrossCompilation.md) --
  authoritative reference covering target triples, sysroots, iserv/TH
  cross-compilation, and troubleshooting
- [Clang Cross-Compilation](https://clang.llvm.org/docs/CrossCompilation.html)
- [QEMU User Mode Emulation](https://www.qemu.org/docs/master/user/main.html)
- [Nix Cross-Compilation](https://nixos.wiki/wiki/Cross_Compiling)
- [GHC #21991 — ARM interworking failures](https://gitlab.haskell.org/ghc/ghc/-/issues/21991)
- [GHC External Interpreter (iserv)](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/external-interpreter)
- [ARM Architecture Reference — Interworking](https://developer.arm.com/documentation/ddi0406/latest/) (ARM ARM, §A4.1.1)
