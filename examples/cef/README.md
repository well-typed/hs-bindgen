# CEF Example

This example generates Haskell bindings for the
[Chromium Embedded Framework (CEF)](https://bitbucket.org/chromiumembedded/cef)
C API using `hs-bindgen`.

## Purpose

CEF is a large C API with features that stress-test `hs-bindgen`:

* Huge type definitions (`cef_types.h` is ~117KB)
* Structs with embedded function pointers (virtual method tables, or vtables)
* Reference counting via `cef_base_ref_counted_t`
* Platform-specific type aliases
* Hundreds of enums and nested structs
* 79 C API headers with deep dependency chains (11 levels)
* Vtable structs that refer to each other across 27 headers

This example is exploratory. Its goal is to find the problems that CEF's header
structure causes.

## Prerequisites

* GHC 9.6.x (or compatible)
* Cabal 3.16.x (or compatible)
* LLVM/Clang 15 (or compatible)
* `curl` (for downloading the CEF binary distribution)
* `tar` with bzip2 support

You do not need to build Chromium from source. The script downloads a pre-built
binary distribution (~287MB) from the Spotify CDN. The distribution contains the
headers and `libcef.so`.

### NixOS

`libcef.so` depends on many system libraries, such as glib, X11 and nss. NixOS
does not put them on the default library path, so use a `nix-shell` expression
that sets `LD_LIBRARY_PATH`:

```bash
nix-shell -E '
  with import <nixpkgs> {};
  let libs = [
    glib nss nspr cups.lib xorg.libX11 xorg.libXcomposite
    xorg.libXdamage xorg.libXext xorg.libXfixes xorg.libXrandr
    libgbm expat libxkbcommon cairo pango systemd alsa-lib
    at-spi2-core dbus xorg.libxcb
  ]; in mkShell {
    buildInputs = libs;
    shellHook = "export LD_LIBRARY_PATH=${lib.makeLibraryPath libs}:$LD_LIBRARY_PATH";
  }
'
```

Start that shell from `nix develop` at the repository root, so that GHC, cabal
and libclang are available too, and run the script in it by adding
`--run ./generate-and-run.sh` to the `nix-shell` call.

## Running

```bash
cd examples/cef
./generate-and-run.sh
```

The script will:

1. Download the CEF binary distribution (if not already present)
2. Generate Haskell bindings for all 79 C API headers in one library-mode run
3. Write `hs-project/cabal.project.paths` with the include and library paths
4. Symlink the resource files from `Resources/` into `Release/`, next to
   `libcef.so`, where CEF looks for them
5. Build and run a minimal Haskell program that loads the bindings

## Library mode

The script generates the bindings with a single run of `hs-bindgen-cli
preprocess --library` on the `include/` directory of the CEF distribution. The
[library mode manual page](../../manual/low-level/usage/library-mode.md)
explains how library mode works. The input headers are the 79 C API headers in
`include/capi/`, plus `include/cef_api_hash.h` and `include/cef_version_info.h`.
Each input header, and each header under `include/` that the input headers
include, gets its own modules, named after its path below `include/`:

| Header | Types module |
|---|---|
| `include/capi/cef_app_capi.h` | `CEF.Capi.Cef_app_capi` |
| `include/internal/cef_types.h` | `CEF.Internal.Cef_types` |
| `include/cef_api_hash.h` | `CEF.Cef_api_hash` |

Next to the types module, a header gets `.Safe`, `.Unsafe` and `.FunPtr`
modules for its functions. To list every file the run writes, add `--dry-run`
to the call in the script.

These bindings differ from bindings generated one header at a time:

* The vtable structs of 27 headers, among them `cef_browser_capi.h`,
  `cef_frame_capi.h` and most handler headers, refer to each other. Their types
  modules form an import cycle, so each of them gets a boot file (`.hs-boot`).
  The modules import each other through the boot files. A plain `cabal build`
  compiles them.
* The `ToFunPtr` and `FromFunPtr` instances for the vtable methods are in
  `CEF.Callback`, a callback module that no generated module imports. Code that
  calls a vtable method needs `import CEF.Callback ()`.
* Other headers use `struct _cef_command_line_t` directly, so the typedef
  `cef_command_line_t` becomes a newtype around the struct. Most other vtable
  typedefs become newtypes for the same reason. `app/Main.hs` shows how to
  reach the vtable.
* `include/internal/cef_thread_internal.h` uses the POSIX types `pid_t` and
  `pthread_t`, which come from system headers outside `include/`. The external
  binding specification `posix-types.yaml` maps them to `CInt` and `CULong`.
  Without it, library mode skips every declaration that uses these types,
  including the thread API of `cef_thread_capi.h`.

## CEF version

The example pins CEF version `145.0.28+g51162e8+chromium-145.0.7632.160`
(linux64 minimal distribution).

## Results

* All 79 C API headers generate bindings
* The run produces 231 Haskell modules and 27 boot files for 99 headers: the 81
  input headers and 18 headers under `include/` that they include
* All generated code compiles and links against `libcef.so`
* The test executable initializes CEF, creates objects with the C API and calls
  vtable methods through the function pointers in the generated structs

Expected output (CEF also prints warnings to stderr, which are normal):

```
CEF initialized successfully.
  API version: 14500
  cef_command_line_create: OK
  is_valid: 1
  has_switches (empty): 0
All CEF binding calls succeeded.
CEF shutdown complete.
```

## Issues log

We hit these issues while developing the bindings.

### 1. `cef_process_util_capi.h`: missing `#include` (CEF translator bug)

The CEF translator generates `cef_process_util_capi.h`. The header declares
`cef_launch_process(struct _cef_command_line_t* command_line)` but includes only
`cef_base_capi.h`. It lacks the `#include` for `cef_command_line_capi.h`. When
the header is compiled on its own, the struct tag first appears inside a
function prototype. The tag then has function prototype scope (C11 6.2.1p4), so
it names a different type from the file-scope `struct _cef_command_line_t`
defined in `cef_command_line_capi.h`. GCC and Clang compile this with a warning.
This is a bug in the CEF translator: the other 78 headers it generates all
include the correct dependencies.

An `hs-bindgen` run on this header alone skips the function:

```
Could not select declaration:
  Declaration of 'struct _cef_command_line_t' will not be visible outside of this function
```

Library mode parses all input headers together. An earlier input header,
`cef_app_capi.h`, includes `cef_command_line_capi.h`, so the struct is already
declared at file scope. `CEF.Capi.Cef_process_util_capi.Safe` therefore binds
`cef_launch_process`.

### 2. API version must be configured via `cef_api_hash`

CEF 145 introduced an API versioning system. `libcef.so` is compiled with
`BUILDING_CEF_SHARED`, which includes every struct field from every API version.
Client code must call `cef_api_hash(CEF_API_VERSION, 0)` **before any other CEF
function**. The call tells `libcef.so` which API version the client was compiled
with. Without it, the CppToC struct wrappers crash with SIGTRAP when they create
an object, for example in `cef_command_line_create`.

The script generates the bindings with the root directive `--hash-define
CEF_API_VERSION 14500`, which also appears in the generated wrapper source. The
test executable calls `cef_api_hash(14500, 0)` before `cef_execute_process`.

`cef_api_hash` is declared in `include/cef_api_hash.h` (not under
`include/capi/`), so its header does not come from the CEF translator. The
script adds that header to the input headers of the library-mode run, so
`cef_api_hash` is bound in `CEF.Cef_api_hash.Safe`, `.Unsafe` and `.FunPtr`.

### 3. CEF runtime dependencies

CEF is a full browser engine, so `libcef.so` depends on many system libraries,
such as glib, nspr, nss, X11, dbus and cups. On Ubuntu these are standard system
packages. On NixOS, see the nix-shell instructions above.
