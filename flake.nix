{
  description = "Automatically generate Haskell bindings from C header files";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    libclang-bindings-src = {
      url = "github:well-typed/libclang?rev=cdbf817187a261ebf8c31b32c85c5693eedf298d";
      flake = false;
    };
    doxygen-parser-src = {
      url = "github:well-typed/doxygen-parser?rev=64faaf3d43599076eb5a04eae7af777de74c7e9b";
      flake = false;
    };
  };

  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      #
      libclang-bindings-src,
      doxygen-parser-src,
      ...
    }:
    let
      overlays = import ./nix/overlay {
        inherit (nixpkgs) lib;
        inherit libclang-bindings-src doxygen-parser-src;
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem = import ./nix/hs-bindgen.nix {
        inherit nixpkgs libclang-bindings-src doxygen-parser-src;
      };
      flake.overlays = overlays;
    };
}
