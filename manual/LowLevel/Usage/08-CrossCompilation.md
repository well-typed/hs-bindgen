# Cross-compilation

`hs-bindgen` can generate bindings for a target platform that differs from the
host platform where the tool runs. This is useful when developing on one
architecture (e.g., x86_64) but deploying to another (e.g., aarch64 or ARM).

## Part A: Generating cross-compiled bindings

This section covers the `hs-bindgen`-specific part: generating Haskell bindings
with correct struct sizes and alignments for a target architecture.

### Why cross-compilation matters for FFI

Cross-compilation affects binding generation because struct sizes and alignments
differ between platforms. For example, `long` is 8 bytes on 64-bit Linux
(LP64) but 4 bytes on 32-bit platforms (ILP32).

### Target triple

The target platform is specified using Clang's `--target` option.  The target
triple format is `<arch>-<vendor>-<os>-<environment>`, where some components may
be omitted.

Common target triples:

| Triple | Description |
|--------|-------------|
| `x86_64-linux-gnu` | 64-bit Linux (x86_64) |
| `aarch64-linux-gnu` | 64-bit Linux (ARM64) |
| `arm-linux-gnueabihf` | 32-bit Linux (ARM, hard float) |
| `i686-linux-gnu` | 32-bit Linux (x86) |
| `x86_64-apple-darwin` | 64-bit macOS (Intel) |
| `aarch64-apple-darwin` | 64-bit macOS (Apple Silicon) |
| `x86_64-pc-windows-msvc` | 64-bit Windows (MSVC) |

Pass the target to `hs-bindgen` using `--clang-option`:

```console
hs-bindgen-cli preprocess \
  --clang-option="--target=aarch64-linux-gnu" \
  -I include \
  --module Foo \
  --hs-output-dir src \
  foo.h
```

To verify the target is being used correctly:

```console
hs-bindgen-cli info libclang \
  --clang-option="--target=aarch64-linux-gnu" \
  --clang-option="-v" \
  2>&1 | grep "^Target:"
```

Expected output:
```
Target: aarch64-unknown-linux-gnu
```

### Target headers (sysroot)

When cross-compiling, the standard headers (e.g., `stdint.h`, `stddef.h`) must
be for the target platform, not the host.  For simple headers that don't include
standard library headers, the target specification alone may be sufficient.
Headers that use types like `size_t` or include `<stdint.h>` require
target-appropriate headers.

#### Nix

On NixOS or with Nix, obtain target headers using `pkgsCross`:

```bash
# Get the aarch64 sysroot (glibc headers)
SYSROOT=$(nix build --print-out-paths \
  'nixpkgs#pkgsCross.aarch64-multiplatform.glibc.dev')

# Generate bindings with target headers
BINDGEN_EXTRA_CLANG_ARGS="" \
BINDGEN_BUILTIN_INCLUDE_DIR="" \
hs-bindgen-cli preprocess \
  --clang-option="--target=aarch64-linux-gnu" \
  --clang-option="-isystem$SYSROOT/include" \
  -I include \
  --module Foo \
  --hs-output-dir src \
  foo.h
```

> [!NOTE]
> When cross-compiling on NixOS/Nix, the `BINDGEN_EXTRA_CLANG_ARGS` environment
> variable set by the Nix dev shell contains host-specific include paths that
> conflict with the target.  Similarly, `BINDGEN_BUILTIN_INCLUDE_DIR` controls
> where `hs-bindgen` looks for Clang's built-in headers (like `stddef.h`); the
> Nix dev shell sets it to prevent heuristic searching, but those paths are
> host-specific.  Clear both when cross-compiling.

Available cross-compilation targets in nixpkgs:

| Nixpkgs attribute | Target |
|-------------------|--------|
| `pkgsCross.aarch64-multiplatform` | 64-bit ARM Linux |
| `pkgsCross.armv7l-hf-multiplatform` | 32-bit ARM Linux (hard float) |
| `pkgsCross.i686-embedded` | 32-bit x86 |
| `pkgsCross.musl64` | 64-bit Linux with musl libc |
| `pkgsCross.mingwW64` | 64-bit Windows (MinGW) |

#### Other systems

On other systems, you can:

1. **Use a cross-compilation toolchain**: Install a cross-compiler (e.g.,
   `aarch64-linux-gnu-gcc`) which typically includes target headers.

2. **Download target sysroot**: Obtain a sysroot containing headers for the
   target platform.

3. **Use Docker/containers**: Run `hs-bindgen` in a container for the target
   architecture.

### Architecture differences example

Consider a C struct with architecture-dependent types:

```c
struct Example {
    long l;      /* 4 bytes on ILP32, 8 bytes on LP64 */
    void *ptr;   /* 4 bytes on 32-bit, 8 bytes on 64-bit */
    int i;       /* always 4 bytes */
    char c;      /* always 1 byte */
};
```

The generated `Storable` instance differs by platform:

**64-bit platform (x86_64, aarch64):**
```haskell
instance F.Storable Example where
  sizeOf = \_ -> (24 :: Int)
  alignment = \_ -> (8 :: Int)
  -- field offsets: l=0, ptr=8, i=16, c=20
```

**32-bit platform (ARM, i686):**
```haskell
instance F.Storable Example where
  sizeOf = \_ -> (16 :: Int)
  alignment = \_ -> (4 :: Int)
  -- field offsets: l=0, ptr=4, i=8, c=12
```

### Working example

A comprehensive working example is available at
[`examples/cross-compilation/`](../../../examples/cross-compilation/). It
includes a Nix development environment, scripts to generate bindings for
multiple targets, and a comparison tool.

```bash
cd examples/cross-compilation
nix develop
./generate-and-run.sh      # generate, build, cross-compile, run
./compare-sizes.sh         # compare struct sizes
```

## Part B: Building and running cross-compiled Haskell executables

Generating bindings with `hs-bindgen` is the first step.  The generated code
must then be compiled with a GHC that produces target-architecture binaries.

### Why standard GHC cannot cross-compile

GHC is a native compiler: its code generator, runtime system (RTS), and
compiled base libraries are all built for one specific target architecture.
A standard GHC installation for x86_64 can only produce x86_64 binaries.

A **cross-compiling GHC** is a separate build of GHC that runs on your host
(e.g., x86_64) but targets a different architecture (e.g., aarch64).  Its
code generator emits target instructions, and its package database contains
libraries compiled for the target.

### Getting a cross-compiling GHC

The easiest way is through **Nix**, which provides pre-built cross-compiling GHC
toolchains and QEMU in a reproducible setup.  Alternatively, you can build GHC
from source (which gives full control but is complex) or use system packages
(rarely available for cross targets).

#### Nix (recommended)

Nix provides pre-built cross-compiling GHC toolchains:

```nix
let
  pkgsAarch64 = pkgs.pkgsCross.aarch64-multiplatform;
  ghcAarch64 = pkgsAarch64.buildPackages.ghc;
  cabalAarch64 = pkgsAarch64.buildPackages.cabal-install;
in ...
```

The [cross-compilation example](../../../examples/cross-compilation/) includes a
complete `flake.nix` that can be adapted for your own projects.

#### Building from source

Follow the guides at <https://log.zw3rk.com/> and the
[GHC Cross-Compilation Wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling)
for detailed instructions.

### Template Haskell and iserv

Template Haskell (TH) splices execute at compile time. During cross-compilation,
the compiled code is for the target architecture and cannot run on the host.
GHC solves this using **iserv** (the external interpreter).

#### What iserv does

When GHC encounters a TH splice during cross-compilation:

1. GHC sends the splice to an iserv process via pipes (using the
   `GHCi.Message` protocol)
2. iserv evaluates the splice and sends the result back
3. GHC incorporates the result into the compiled output

The iserv binary is a **target-architecture executable** that runs under QEMU.
This allows TH code to execute in the target environment while GHC runs on the
host.

#### GHC options

| Option | Purpose |
|--------|---------|
| `-fexternal-interpreter` | Use iserv instead of built-in interpreter |
| `-pgmi <wrapper>` | Path to the iserv wrapper script |

The wrapper script invokes iserv under QEMU:

```bash
#!/usr/bin/env bash
exec qemu-aarch64 -L /path/to/sysroot /path/to/iserv "$@"
```

GHC passes iserv's arguments (pipe file descriptors) to the wrapper.

> [!IMPORTANT]
> The `-fexternal-interpreter` and `-pgmi` options must apply to **all** packages,
> not just your package.  Dependencies like `hs-bindgen-runtime` use TH and need
> these options too.  Set them in `cabal.project.local` under `package *`:
>
> ```cabal
> package *
>   ghc-options: -fexternal-interpreter -pgmi /path/to/iserv-wrapper.sh
> ```

#### Why build iserv from source

Nix's cross-GHC (`pkgsCross.*.buildPackages.ghc`) does not ship an iserv binary
(`ghc-iserv` or `ghc-iserv-dyn`).  The `libiserv` library and `GHCi.Utils`
module are not in the cross-GHC's package database -- only the `ghci` package is
available (providing `GHCi.Run`, `GHCi.Message`, `GHCi.TH`, etc.).

The [cross-compilation example](../../../examples/cross-compilation/) script
inlines the necessary source from GHC 9.6.6
([`utils/iserv/`](https://gitlab.haskell.org/ghc/ghc/-/tree/master/utils/iserv)
and [`libraries/libiserv/`](https://gitlab.haskell.org/ghc/ghc/-/tree/master/libraries/libiserv))
and compiles it with the cross-GHC.  Specifically, it inlines:

- **`GHCi.Utils.getGhcHandle`**: converts a C file descriptor to a Haskell
  `Handle`.  This module exists in the `ghci` package source but is not exposed
  in its public API.

- **`IServ.serv`**: the main message loop from `libiserv`.  It reads messages
  from the GHC pipe, dispatches TH evaluation via `GHCi.TH.runTH`, and sends
  results back.

#### Why iserv needs a custom C main (`iservmain.c`)

GHC's standard Haskell `main` entry point does not allow customising the RTS
configuration before startup.  iserv requires a custom C `main()` compiled with
`-no-hs-main` for two reasons:

1. **`keep_cafs = 1`**: normally GHC's runtime garbage-collects CAFs (Constant
   Applicative Forms -- top-level thunks like `x = expensiveComputation`) after
   evaluation.  But iserv interprets code across multiple interactions: a TH
   splice evaluated in one round may be referenced by a later splice.  Without
   `keep_cafs`, the RTS would GC those results, causing crashes from dangling
   pointers.  See
   [`rts/sm/Storage.c`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/sm/Storage.c)
   (`revertCAFs`).

2. **`rts_opts_enabled = RtsOptsAll`**: allows passing RTS options (e.g.,
   `+RTS -M` for heap size) to iserv for debugging.

This pattern comes from GHC's own iserv build:
[`utils/iserv/cbits/iservmain.c`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/utils/iserv/cbits/iservmain.c).

> [!TIP]
> The `generate-and-run.sh` script handles iserv building, wrapper creation,
> and cabal configuration automatically.  See the script for the complete
> implementation.

### Configuring cabal for cross-compilation

Once you have a cross-compiling GHC, configure cabal to use it:

**Command-line:**
```bash
cabal build your-executable \
  --with-compiler=/path/to/aarch64-linux-gnu-ghc \
  --with-ghc-pkg=/path/to/aarch64-linux-gnu-ghc-pkg \
  --builddir=dist-aarch64 \
  --extra-lib-dirs=/path/to/aarch64/c-libraries \
  --extra-include-dirs=/path/to/c-headers
```

**cabal.project.local:**
```cabal
with-compiler: /path/to/aarch64-linux-gnu-ghc
with-ghc-pkg: /path/to/aarch64-linux-gnu-ghc-pkg

package your-package
  extra-lib-dirs: /path/to/aarch64/c-libraries
  extra-include-dirs: /path/to/c-headers
```

> [!TIP]
> Use `--builddir` to keep cross-compiled artifacts separate from native builds.

### hsc2hs and cross-compilation

[`hsc2hs`](https://downloads.haskell.org/ghc/latest/docs/users_guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs)
is a GHC preprocessor that processes `.hsc` files into `.hs` files.  It handles
directives like `#{size struct foo}`, `#{alignment struct foo}`, and
`#{peek struct foo, bar}` by compiling a small C program that prints the values,
running it, and substituting the results into the Haskell source.

`hs-bindgen-runtime` uses `.hsc` files, so `hsc2hs` is needed during the build.

`hsc2hs` is a **build-time tool**: it runs on the host to generate Haskell
source code.  It does not need to be cross-compiled.  However, Nix's cross-GHC
ships a target-prefixed `hsc2hs` that sometimes fails cabal's version check.
In that case, use the native `hsc2hs` as a fallback:

```bash
cabal build your-executable \
  --with-compiler="$GHC_AARCH64_PATH" \
  --with-hsc2hs="$(command -v hsc2hs)" \
  ...
```

> [!NOTE]
> The `generate-and-run.sh` script handles this automatically: it tries the
> cross-GHC's `hsc2hs` first and falls back to the native one if needed.

### Running cross-compiled binaries with QEMU

Cross-compiled binaries won't run directly on your host machine. Use QEMU
user-mode emulation:

```bash
# With sysroot (for system library access)
qemu-aarch64 -L /path/to/sysroot ./your-app

# With custom library path
LD_LIBRARY_PATH=./lib-aarch64 \
  qemu-aarch64 -L /path/to/sysroot \
  -E LD_LIBRARY_PATH=./lib-aarch64 \
  ./your-app
```

The `-L` flag tells QEMU where to find the target's system libraries (glibc,
ld-linux, etc.).

### Troubleshooting

#### Environment variable conflicts

On NixOS, clear `BINDGEN_EXTRA_CLANG_ARGS` when cross-compiling:

```bash
BINDGEN_EXTRA_CLANG_ARGS="" hs-bindgen-cli preprocess ...
```

#### `libclang.so` not found at compile time

Error: `cannot open shared object file: No such file or directory` (referring to
`libclang.so`)

`hs-bindgen` loads `libclang.so` dynamically.  When building packages that use
Template Haskell (e.g., `hs-bindgen` itself), GHC's linker must find
`libclang.so` at compile time.  Set `LD_LIBRARY_PATH` to include the directory
containing `libclang.so`:

```bash
export LD_LIBRARY_PATH=/usr/lib/llvm-18/lib:$LD_LIBRARY_PATH
```

The Nix dev shells in the [cross-compilation example](../../../examples/cross-compilation/)
handle this automatically.

#### Missing target headers

Error: `fatal error: 'stdint.h' file not found`

Add target-appropriate system headers:

```console
--clang-option="-isystem/path/to/target/sysroot/include"
```

#### Target triple not recognized

Error: `error: unknown target triple 'invalid-triple'`

Check available targets:

```console
clang --print-targets
```

#### ABI differences

Some architectures have multiple ABIs (e.g., ARM hard-float vs soft-float).
Ensure the target triple matches the actual ABI used by the C library.

| ABI | Target Triple |
|-----|---------------|
| ARM hard-float | `arm-linux-gnueabihf` |
| ARM soft-float | `arm-linux-gnueabi` |

#### QEMU errors

**ELF interpreter missing** (`No such file or directory`):
Use `-L` to specify the sysroot containing the target's dynamic linker.

**Library not found** (`error while loading shared libraries: libfoo.so`):
Pass `LD_LIBRARY_PATH` to QEMU with `-E`:
```bash
qemu-aarch64 -L /sysroot -E LD_LIBRARY_PATH=./lib-aarch64 ./your-app
```

**Wrong architecture** (`Illegal instruction`):
Verify the binary architecture with `file ./your-app`.

## References

- [GHC Cross-Compilation Wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling)
- [zw3rk blog -- GHC cross-compilation tutorials](https://log.zw3rk.com/)
- [Well-Typed: Improving GHC configuration and cross-compilation](https://well-typed.com/blog/2023/10/improving-ghc-configuration-and-cross-compilation-with-ghc-toolchain/)
- [GHC External Interpreter wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/external-interpreter)
- [Cross-compilation example](../../../examples/cross-compilation/)
- [Clang Cross-Compilation](https://clang.llvm.org/docs/CrossCompilation.html)
- [QEMU User Mode Emulation](https://www.qemu.org/docs/master/user/main.html)
