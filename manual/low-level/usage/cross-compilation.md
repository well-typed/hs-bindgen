# Cross-compilation

`hs-bindgen` can generate bindings for a target platform that differs from the
host platform where the tool runs. This is useful when developing on one
architecture (e.g., x86_64) but deploying to another (e.g., aarch64 or ARM).

## Generating cross-compiled bindings

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
cabal run hs-bindgen-cli -- preprocess \
  --clang-option="--target=aarch64-linux-gnu" \
  -I include \
  --module Foo \
  --hs-output-dir src \
  foo.h
```

To verify the target is being used correctly:

```console
cabal run hs-bindgen-cli -- info libclang \
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

> [!TIP]
> Nix's cross-compiler CC wrappers (`llvmPackages.stdenv.cc` for Clang,
> `buildPackages.gcc` for GCC) include target sysroot headers automatically --
> no `-isystem` needed when using them for C compilation (e.g., `make`).
> The explicit `-isystem` approach shown above is needed when using
> `hs-bindgen-cli`'s `--target` flag, which goes through libclang directly
> (not via the CC wrapper), or when using a raw compiler without a wrapper.

> [!NOTE]
> When cross-compiling on NixOS/Nix, the `BINDGEN_EXTRA_CLANG_ARGS` environment
> variable set by the `hs-bindgen` Nix development shell (see `flake.nix`)
> contains host-specific include paths that conflict with the target. Similarly,
> `BINDGEN_BUILTIN_INCLUDE_DIR` controls where `hs-bindgen` looks for Clang's
> built-in headers (like `stddef.h`); the `hs-bindgen` development shell sets it
> to prevent heuristic searching, but those paths are host-specific. These
> variables are specific to `hs-bindgen`'s Nix setup, not a general Nix
> mechanism. Clear both when cross-compiling.

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

3. **Use Docker/containers**: Run `hs-bindgen` in a target-architecture
   container (via QEMU system emulation). Note: this is native compilation
   under emulation, not true cross-compilation -- `hs-bindgen` sees the
   container's native architecture and generates bindings accordingly.

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
[`examples/cross-compilation/`][example:cross-compilation]. It includes a Nix
development environment and a script to generate bindings, cross-compile, and
run under QEMU.

```bash
cd examples/cross-compilation
nix develop
./generate-and-run.sh    # generate bindings, cross-compile, run under QEMU
```

## Part B: Building and running cross-compiled Haskell executables

Generating bindings with `hs-bindgen` is the first step. The generated code
must then be compiled with a GHC that produces target-architecture binaries.

### Why standard GHC cannot cross-compile

GHC is a native compiler: its code generator, runtime system (RTS), and
compiled base libraries are all built for one specific target architecture.
A standard GHC installation for x86_64 can only produce x86_64 binaries.

A **cross-compiling GHC** is a separate build of GHC that runs on your host
(e.g., x86_64) but targets a different architecture (e.g., aarch64). Its
code generator emits target instructions, and its package database contains
libraries compiled for the target.

### Getting a cross-compiling GHC

The easiest way is through **Nix**, which provides pre-built cross-compiling GHC
toolchains and QEMU in a reproducible setup. Alternatively, you can build GHC
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

The [cross-compilation example][example:cross-compilation] includes a complete
`flake.nix` that can be adapted for your own projects.

#### Building from source

Follow the guides the [GHC Cross-Compilation Wiki][ghc:wiki:cross-compiling] for
detailed instructions.

### Cross-compiling C compiler

Cross-compiling C libraries (that your Haskell FFI code links against) requires
a C compiler that targets the right architecture. This is separate from GHC's
cross-compilation -- GHC's cross-compiler has its own C compiler (typically GCC)
hardcoded in its settings file, which it uses internally for linking and RTS
compilation. You do not need to configure GHC's internal C compiler.

The [cross-compilation example][example:cross-compilation] uses Clang via Nix's
`pkgsAarch64.llvmPackages.stdenv.cc` for consistency with `hs-bindgen-cli`,
which is built on libclang/LLVM. This is a Nix CC wrapper with the target
sysroot headers, linker paths, and `--target` flag baked in, so
`make CC="$AARCH64_CC"` works without extra flags.

Non-Nix users can use any cross-compiler for their C libraries; Clang is
recommended for consistency with `hs-bindgen-cli` (which is built on
libclang/LLVM). See the
[Clang cross-compilation documentation][clang:docs:cross-compilation] for
details on configuring `--target` and sysroot paths.

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

#### Getting iserv

If you build GHC from source using Hadrian's default settings, iserv
(`ghc-iserv` / `ghc-iserv-dyn`) is included in the installation and no extra
steps are needed -- just point `-pgmi` at the wrapper script.

Nix's cross-GHC (`pkgsCross.*.buildPackages.ghc`), however, does not ship an
iserv binary. However, the `ghci` package is available in the cross-GHC's
package database and provides `GHCi.Server.defaultServer`, which is the entire
iserv implementation. Building iserv from source is just compiling a 4-line
module:

```haskell
module Main (main) where
import GHCi.Server (defaultServer)
main :: IO ()
main = defaultServer
```

We compile with `-rtsopts=all` to allow passing RTS options (e.g., `+RTS -M`
for heap size) to iserv for debugging.

> [!NOTE]
> iserv handles CAF (Constant Applicative Form) retention internally: when GHC
> sends the `InitLinker` message, iserv calls `initObjLinker RetainCAFs`, which
> tells the RTS object linker to retain CAFs for dynamically loaded code (TH
> splices). This is a different mechanism from the `-fkeep-cafs` compile-time
> flag, which prevents GC of a binary's own statically-linked CAFs. iserv does
> not need `-fkeep-cafs`.

The [cross-compilation example](../../../examples/cross-compilation/) script
compiles this module with the cross-GHC, producing a target-architecture iserv
binary that runs under QEMU.

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

Nix's cross-GHC ships `hsc2hs` under a target-prefixed name (e.g.,
`aarch64-unknown-linux-gnu-hsc2hs`), which cabal does not discover
automatically. Pass it explicitly with `--with-hsc2hs`:

```bash
cabal build your-executable \
  --with-compiler="$GHC_AARCH64_PATH" \
  --with-hsc2hs="${GHC_AARCH64_PATH%-ghc}-hsc2hs" \
  ...
```

> [!NOTE]
> The `generate-and-run.sh` script handles this automatically.

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

## Part C: Cross-compilation in Template Haskell mode

Parts A and B describe **preprocess mode**: `hs-bindgen-cli` runs on the host
to generate `.hs` files ahead of time, then a cross-GHC compiles them. In
**Template Haskell (TH) mode**, the user's package contains a
`withHsBindgen` / `hashInclude` splice that generates the bindings *during*
compilation. This section covers the additional considerations for TH-mode
cross-compilation.

> [!TIP]
> Preprocess mode is the recommended path for cross-compilation. TH mode
> works but is structurally heavier: the full `hs-bindgen` package must be
> cross-built, and a target-architecture `libclang.so` must be reachable
> from the iserv runtime.

### Why TH-mode cross-compilation is different

A cross-compiling GHC delegates TH splices to **iserv**, a target-architecture
binary running under QEMU (covered in Part B). The splice itself therefore
executes in the *target* environment, not on the host:

1. GHC encounters `withHsBindgen ... $ hashInclude "foo.h"` and sends the
   splice to iserv.
2. Inside iserv, the GHC RTS object linker loads the compiled `hs-bindgen`
   object code (target architecture).
3. That code dynamically loads `libclang.so` to parse the C header.
4. iserv returns the generated declarations; GHC compiles them into the
   target binary.

This has two practical consequences:

- **The full `hs-bindgen` package is cross-built**, not just
  `hs-bindgen-runtime`. In preprocess mode the user's package depends only on
  `hs-bindgen-runtime` (which is small); in TH mode it depends on
  `hs-bindgen` itself, which transitively pulls in `libclang-bindings`,
  `doxygen-parser`, and so on. The first cross-build is therefore
  significantly heavier.
- **A target-architecture `libclang.so` must be reachable from iserv.** The
  `libclang-bindings` package declares `extra-libraries: clang`, so when
  iserv loads `hs-bindgen`'s object code it dlopens `libclang.so`. That
  `libclang.so` must be the target architecture and on the QEMU dynamic
  linker's search path.

### Cabal configuration

The user's package needs to depend on the full `hs-bindgen` package (not just
`hs-bindgen-runtime`) and enable the language extensions the splice-generated
code uses. A practical baseline is `default-language: GHC2021` together with
the extension list used by hs-bindgen's own TH tests
([`hs-bindgen/test/th/Test/TH/Test01.hs`](../../../hs-bindgen/test/th/Test/TH/Test01.hs)).
GHC2021 matters because the splice expands inside the user's module rather
than a separate generated file, so the user's `default-extensions` need to
cover everything the generated code uses:

```cabal
executable my-app
  build-depends:      base, hs-bindgen, hs-bindgen-runtime
  default-language:   GHC2021
  default-extensions:
      CApiFFI
      DataKinds
      DerivingStrategies
      DerivingVia
      MagicHash
      OverloadedLabels
      OverloadedStrings
      PatternSynonyms
      TemplateHaskell
      TypeFamilies
      UnboxedTuples
      UndecidableInstances
```

The `-fexternal-interpreter -pgmi <wrapper>` ghc-options from Part B still
apply here, because `hs-bindgen` itself uses TH and must go through iserv.
Set them on `package *`:

```cabal
package *
  ghc-options: -fexternal-interpreter -pgmi /path/to/iserv-wrapper.sh
```

#### Linking the wrapped C library: `extra-libraries`

Each `foreign import ccall foo :: ...` declaration causes GHC to emit a
small C stub (in `dist-newstyle/.../*-tmp/ghc_*.c`) that calls into the
underlying C symbol. That stub is compiled into the same object file as the
module containing the import. In TH mode the splice expands inside the
user's module, so the stubs end up in `Main.o` itself, alongside `main`,
which means the final link must resolve the C symbols. Declare the wrapped
library explicitly:

```cabal
executable my-app
  extra-libraries: my_c_lib
```

### Reaching target `libclang.so` from QEMU

Augment the iserv wrapper script to set `LD_LIBRARY_PATH` (via QEMU's `-E`
flag) so the RTS object linker inside iserv can find target `libclang.so`:

```bash
#!/usr/bin/env bash
exec qemu-aarch64 \
    -L /path/to/target/sysroot \
    -E "LD_LIBRARY_PATH=/path/to/target/libclang/lib:/path/to/target/gmp/lib:/path/to/target/c-libs" \
    /path/to/target/iserv "$@"
```

On Nix, the target libclang is exposed by `pkgsCross.<target>.llvmPackages.libclang.lib`:

```nix
let
  pkgsAarch64 = pkgs.pkgsCross.aarch64-multiplatform;
  aarch64Libclang = pkgsAarch64.llvmPackages.libclang.lib;
in ...
```

### TH splice include paths

The splice's C include directories must be passed explicitly via its
`Config`, they are *not* derived from cabal's `extra-include-dirs`:

```haskell
import HsBindgen.TH

let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "../c-src"]
    cfgTh :: ConfigTH
    cfgTh = def
 in withHsBindgen cfg cfgTh $
      hashInclude "arch_types.h"
```

`Pkg` paths are resolved relative to the package root (the directory
containing the `.cabal` file). `Dir` accepts an absolute path. Cross- and
host-target flags (e.g. `--target=aarch64-linux-gnu`) can be passed via
`Config`'s clang fields.

### Transitive dependency workarounds

Two of `hs-bindgen`'s transitive dependencies need a small workaround when
cross-building manually (cross-GHC + target-arch package set, no nixpkgs
Haskell wrapper):

#### `zlib`: `Stream.hsc:...: fatal error: zlib.h: No such file or directory`

The Hackage [`zlib`](https://hackage.haskell.org/package/zlib) package
wraps the C zlib library. Its `.hsc` files do `#include <zlib.h>`, and
the cabal preprocessor invokes the cross-CC to expand them. With the
default `pkg-config` flag (`flag pkg-config { default: True }` in
zlib.cabal) cabal asks pkg-config for the include path, but pkg-config
in a typical cross setup is not target-aware -- it either fails or
returns host paths -- so the preprocessing fails with `zlib.h: No such
file or directory`.

The simplest fix is to make the target zlib's headers visible to all
packages:

```cabal
package *
  extra-include-dirs: /path/to/target/zlib/include
  extra-lib-dirs:     /path/to/target/zlib/lib
```

On Nix the include dir comes from `pkgsCross.<target>.zlib.dev` and the
lib dir from `pkgsCross.<target>.zlib.out`.

If a target zlib is inconvenient to obtain, an alternative is the
package's `bundled-c-zlib` flag, which swaps the system-library
dependency for the
[`zlib-clib`](https://hackage.haskell.org/package/zlib-clib) package
(zlib's C source, compiled inline -- no system zlib needed):

```cabal
package zlib
  flags: +bundled-c-zlib
```

(or, on the cabal CLI: `--constraint='zlib +bundled-c-zlib'`).

#### `libclang-bindings`: `Cannot find libclang headers`

`libclang-bindings` is `build-type: Configure` and ships an autoconf
`configure.ac` that locates LLVM via the standard
[`llvm-config`](https://llvm.org/docs/CommandGuide/llvm-config.html)
utility:

```
AC_PATH_PROG([LLVM_CONFIG],[llvm-config],[])
```

If only the host `llvm-config` is on `PATH`, the subsequent
`AC_CHECK_HEADER([clang-c/Index.h])` checks against host include paths and the
configure step fails with a "Cannot find libclang headers" error.

The fix is to provide a target-aware `llvm-config` for the build. Write a
small stub that returns the target paths, name it exactly `llvm-config`,
place it in a dedicated directory, and prepend that directory to `PATH` so
`AC_PATH_PROG`'s normal search picks it up before the host's:

```bash
mkdir -p llvm-stub-bin
cat > llvm-stub-bin/llvm-config << 'EOF'
#!/usr/bin/env bash
case "$1" in
    --version)    echo "19.1.7" ;;
    --libdir)     echo "/path/to/target/libclang/lib" ;;
    --includedir) echo "/path/to/target/libclang/include" ;;
    --prefix)     echo "/path/to/target/libclang" ;;
esac
EOF
chmod +x llvm-stub-bin/llvm-config

PATH=$(pwd)/llvm-stub-bin:$PATH cabal build ...
```

The configure script also declares

```
AC_ARG_VAR(LLVM_CONFIG, [Location of llvm-config])
```

so as a fallback you can set `LLVM_CONFIG=/path/to/stub`
([`AC_ARG_VAR`](https://www.gnu.org/software/autoconf/manual/html_node/Setting-Output-Variables.html)
is autoconf's "precious" env-var mechanism, which overrides any
`AC_PATH_PROG` search). The `PATH` approach above is preferred because it
keeps the configure flow on its normal path and avoids reserving an env
var for this single purpose.

The four flags above are the subset that our consumers actually call:

- `--version` -- libclang-bindings's configure.ac sanity check (output
  unused beyond a non-empty check; any string works)
- `--libdir` -- `libclang-bindings` configure.ac: `LDFLAGS=-L<libdir>`
- `--includedir` -- `libclang-bindings` configure.ac:
  `CPPFLAGS=-I<includedir>`
- `--prefix` -- consumed by hs-bindgen's clang-builtin-headers discovery
  (see next subsection); only exercised when
  `BINDGEN_BUILTIN_INCLUDE_DIR != "disable"`

On Nix, the target paths come from
`pkgsCross.<target>.llvmPackages.libclang.{lib,dev}`. The `.dev` output
provides the headers that go in `--includedir`; the `.lib` output
provides the `.so` files that go in `--libdir`.

#### Skipping clang's builtin-headers discovery

When the splice runs, hs-bindgen tries to locate clang's builtin
include directory. In a normal install this lives at
`<llvm-prefix>/lib/clang/<ver>/include/`.

The discovery flow (see
[`hs-bindgen/src-internal/HsBindgen/Clang/BuiltinIncDir.hs`](../../../hs-bindgen/src-internal/HsBindgen/Clang/BuiltinIncDir.hs))
checks, in order:

1. The `BINDGEN_BUILTIN_INCLUDE_DIR` environment variable
   (`disable` skips discovery; `clang` selects the discovery path below)
2. `$(${LLVM_CONFIG} --prefix)/bin/clang -print-resource-dir` then
   `<resource-dir>/include`
3. `$(clang -print-resource-dir)/include` from `PATH`

If your header has no system `#include`s, the splice does not need any of
these, discovery falls through to host-clang and the splice still produces
correct bindings. Setting `BINDGEN_BUILTIN_INCLUDE_DIR=disable` is still
recommended though: it skips discovery entirely, so the splice cannot
accidentally pick up host builtin includes that would be architecturally
wrong if the header started using `<stdint.h>` etc.

### Working example

The [cross-compilation example](../../../examples/cross-compilation/)
ships both modes side-by-side. Phase 4 of `generate-and-run.sh` cross-builds
and runs the TH executable under QEMU; on success the output reports the
target struct sizes from the TH splice next to the same values measured by
`hsc2hs`, and the two columns match.

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

The Nix dev shells in the [cross-compilation example][example:cross-compilation]
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

- [GHC Cross-Compilation Wiki][ghc:wiki:cross-compiling]
- [zw3rk blog -- GHC cross-compilation tutorials][blog:zw3rk]
- [Well-Typed: Improving GHC configuration and cross-compilation][]
- [GHC External Interpreter wiki][ghc:wiki:external-interpreter]
- [Cross-compilation example][example:cross-compilation]
- [Clang Cross-Compilation][clang:docs:cross-compilation]
- [QEMU User Mode Emulation][qemu:docs:user-mode-emulation]


<!-- sources and references -->

[blog:zw3rk]: https://log.zw3rk.com/
[clang:docs:cross-compilation]: https://clang.llvm.org/docs/CrossCompilation.html
[example:cross-compilation]: ../../../examples/cross-compilation
[ghc:wiki:cross-compiling]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling
[ghc:wiki:external-interpreter]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/external-interpreter
[qemu:docs:user-mode-emulation]: https://www.qemu.org/docs/master/user/main.html
[Well-Typed: Improving GHC configuration and cross-compilation]: https://well-typed.com/blog/2023/10/improving-ghc-configuration-and-cross-compilation-with-ghc-toolchain/
