{
  description = "hs-bindgen development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              # See ./test/internal/Test/Internal/Rust.hs::rustBindgenVersion.
              rust-bindgen-unwrapped = prev.rust-bindgen-unwrapped.overrideAttrs (old: rec {
                version = "0.70.1";
                src = prev.fetchCrate {
                  pname = "bindgen-cli";
                  inherit version;
                  hash = "sha256-6FRcW/VGqlmLjb64UYqk21HmQ8u0AdVD3S2F+9D/vQo=";
                };
                cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
                  pname = old.pname;
                  inherit version src;
                  hash = "sha256-r4ZI+uybK3MzJMYlRwmNhZMBO3aMKCIIznOOdQ0ReqU=";
                };
              });
            })
          ];
        };
        hpkgs = pkgs.haskellPackages;
        lpkgs = pkgs.llvmPackages;
      in
      {
        devShells.default = (pkgs.mkShell.override { stdenv = pkgs.clangStdenv; }) {
          buildInputs = [ lpkgs.libclang ];
          packages = [
            # Haskell.
            hpkgs.cabal-install
            hpkgs.ghc
            hpkgs.haskell-language-server

            # Other.
            lpkgs.clang
            lpkgs.llvm
            pkgs.rust-bindgen
          ];
          shellHook = ''
            # When running `clang` during GHC compilation with Template Haskell,
            # the Nixpkgs wrapper does not work, and so we need to set include
            # paths manually.
            C_INCLUDE_PATH="$(< ${lpkgs.clang}/nix-support/orig-libc-dev)/include:$(< ${pkgs.gcc}/nix-support/orig-cc)/lib/gcc/x86_64-unknown-linux-gnu/14.2.1/include"
            export C_INCLUDE_PATH
            # `rust-bindgen` allows usage of environment variables to define
            # clang arguments which seems a bit more elegant but does not work
            # for `hs-bindgen`. See `rust-bindgen-hook.sh` in Nixpkgs.
          '';
        };
      }
    );
}
