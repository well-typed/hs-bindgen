# Botan Haskell Bindings

This example demonstrates generating Haskell bindings for
[Botan](https://botan.randombit.net/), a cryptography library written in C++.

## Prerequisites

Install ccache on your system. For example, on NixOS:

```bash
nix-shell -p ccache
```

Or, for example, on Ubuntu:

```bash
apt-get install ccache
```

TODO: on Windows, use sccache instead

## Running the Example

```bash
./generate-and-run.sh
```

This script will:

1. Build the botan-3 C++ library
2. Generate Haskell bindings using `hs-bindgen`
3. Create `cabal.project.local` with the necessary configuration
4. Build and run the example program
