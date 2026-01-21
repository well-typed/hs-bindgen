# RPM Haskell Bindings

This example demonstrates generating Haskell bindings for
[RPM Package Manager](https://github.com/rpm-software-management/rpm), a
powerful package management system.

## Prerequisites

### Using Nix Shell

This example includes a `shell.nix` file that provides all necessary
dependencies for building RPM from source. The shell environment includes:

**Build tools:**
- `cmake` - Build system generator
- `pkg-config` - Helper tool for compile/link flags
- `scdoc` - Documentation generator

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

If not using Nix at all, take a look at the [INSTALL](./rpm/INSTALL) file for more details
on the instructions of how to build `rpm`.

## Running the Example

### Building RPM from Source

According to the instructions in [INSTALL](./rpm/INSTALL):

```sh
# On the `rpm` folder
mkdir _build
cd _build
cmake ..
make
make install
```

Once all these steps have completed successfully the libraries will be under
`rpm/_build/_install/lib64`.

### Linking Against the C Library

The `.cabal` file includes `extra-libraries: rpm, rpmio`, which tells the
linker to link against both `librpm.so` and `librpmio.so`. RPM is split into
multiple libraries, and both are needed for the bindings to work correctly.

### Generating the bindings

```bash
./generate-and-run.sh
```

### Generating the Include Graph

This script will already generate the bindings by their right order, i.e. the
header include dependency order. If for any reason the `rpm` library updated,
then it might be necessary to change the `generate-and-run` script so that it
follows the right order.


To visualize the header dependencies, you can generate an include graph using
`hs-bindgen-cli`:

```bash
cd examples/c-rpm
cabal run hs-bindgen-cli -- info include-graph \
    -I rpm/include \
    -o output-graph.md \
    rpm/rpmlib.h
```

This generates a Mermaid diagram showing all header includes and their
dependencies. You can visualize it at [mermaid.live](https://mermaid.live/) or
in any Markdown viewer that supports Mermaid diagrams (like GitHub).
