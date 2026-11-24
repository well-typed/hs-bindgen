# RPM Haskell Bindings

This example demonstrates generating Haskell bindings for the C libraries of the
[RPM Package Manager](https://github.com/rpm-software-management/rpm).

## Prerequisites

### Using Nix Shell

The `shell.nix` file of this example provides the dependencies needed to build
RPM from source:

**Build tools:**
- `cmake` - Build system generator
- `pkg-config` - Helper tool for compile/link flags
- `scdoc` - Documentation generator
- `gettext` - Message catalog tools (`msgfmt`, `msgmerge`) that RPM's cmake
  configuration requires

**RPM dependencies:**
- `popt` - Command line option parsing
- `libarchive` - Multi-format archive and compression library
- `sqlite` - Database engine for RPM database
- `zstd` - Fast compression algorithm
- `elfutils` - ELF object file access library
- `xz` - LZMA compression utilities
- `lua` - Scripting language for RPM
- `zlib` - Compression library
- `bzip2` - Compression library
- `readline` - Command line editing
- `file` - File type identification
- `libcap` - POSIX capabilities library
- `audit` - Linux audit framework
- `libselinux` - SELinux runtime library
- `dbus` - Message bus system
- `rpm-sequoia` - OpenPGP implementation (from nixos-unstable)

> [!TIP]
>
> If you are using Nix or NixOS, please also have a look at our [`hs-bindgen`
> Nix tutorial](https://github.com/well-typed/hs-bindgen-tutorial-nix).

If you do not use Nix, see the
[INSTALL](https://github.com/rpm-software-management/rpm/blob/rpm-6.1.0-rc1/INSTALL)
file for how to build `rpm`.

## Running the Example

### Building RPM from Source

`generate-and-run.sh` clones RPM at tag `rpm-6.1.0-rc1` into `rpm/` and builds
it in `rpm/_build` with `cmake` and `make`, without installing it. The Haskell
project links against the libraries in `rpm/_build/rpmio` and
`rpm/_build/build`. To pass extra options to `cmake`, set `RPM_CMAKE_FLAGS`. CI
uses `-DWITH_SEQUOIA=OFF -DENABLE_PYTHON=OFF`.

### Linking Against the C Library

The `.cabal` file includes `extra-libraries: rpmbuild, rpmio`, which tells the
linker to link against `librpmbuild.so` and `librpmio.so`. RPM is split into
several libraries, and the bindings need both of them.

### Generating the Bindings

From the repository root, start the dev shell, which provides GHC, cabal and
libclang. Then run the script inside this example's `shell.nix`, which provides
the C dependencies:

```bash
nix develop
cd examples/c-rpm
nix-shell --run ./generate-and-run.sh
```

Without Nix, install the dependencies listed above and run
`./generate-and-run.sh` from this directory.

The script generates bindings for 13 RPM headers with a single `hs-bindgen-cli`
run in library mode (`--library rpm/include/rpm`). Each header gets its own
modules, named after the header. For
example, `argv.h` becomes `RPM.Argv`, `RPM.Argv.Safe`, `RPM.Argv.Unsafe` and
`RPM.Argv.FunPtr`, and `rpmtypes.h` becomes `RPM.Rpmtypes`. See the [library
mode manual page](../../manual/low-level/usage/library-mode.md) for how library
mode works.

Some RPM declarations use system types from glibc, such as `off_t`, `ssize_t`
and `struct timeval`. The script passes `--enable-program-slicing`, so the run
also generates these types, into one module, `RPM.Vendor`. Without the flag,
the run would skip the RPM declarations that use them, such as `Fread` and the
stopwatch functions of `rpmsw.h`. `RPM.Vendor` holds copies of the system
types: to share them with other libraries, use an external binding
specification instead, as the [CEF example](../cef) does.
