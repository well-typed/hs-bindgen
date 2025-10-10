# Generate `libpcap` Haskell bindings with `hs-bindgen`

This example demonstrates generating Haskell bindings for
[libpcap](https://github.com/the-tcpdump-group/libpcap), an interface to various
kernel packet capture mechanisms.

The project structure is similar to the example on [MiniSat](../c-minisat); see the
[MiniSat example README](../c-minisat/README.md).

## Prerequisites

Install the build requirements `cmake`, `flex`, and `bison`. For example, on
NixOS:

```bash
nix-shell -p cmake flex bison
```

> [!TIP]
>
> If you are using Nix or NixOS, please also have a look at our [`hs-bindgen`
> Nix tutorial](https://github.com/well-typed/hs-bindgen-tutorial-nix).

## Run the example

```bash
./generate-and-run.sh
```

This script will:
1. Build `libpcap`
2. Create symbolic links for the shared library
3. Generate Haskell bindings using `hs-bindgen`
4. Create `cabal.project.local`
5. Build and run the example program
