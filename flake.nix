{
  description = "Automatically generate Haskell bindings from C header files";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    libclang-bindings-src = {
      url = "github:well-typed/libclang?rev=363bc3593b18670b497c36f29d11ad49df9eea4d";
      flake = false;
    };
  };

  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      #
      libclang-bindings-src,
      ...
    }:
    let
      overlays = import ./nix/overlay {
        inherit (nixpkgs) lib;
        inherit libclang-bindings-src;
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
        inherit nixpkgs libclang-bindings-src;
      };
      flake.overlays = overlays;
    };
}
