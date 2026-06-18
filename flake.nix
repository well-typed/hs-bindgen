{
  description = "Automatically generate Haskell bindings from C header files";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    libclang-bindings-src = {
      url = "github:well-typed/libclang-bindings?rev=ef67437045c45a32d7d1aa7166043937c1259d7f";
      flake = false;
    };
    c-expr-src = {
      url = "github:well-typed/c-expr?rev=c789b4ab20cc13696a96bf4e508a5af95d523ce0";
      flake = false;
    };
    doxygen-parser-src = {
      url = "github:well-typed/doxygen-parser?rev=33b5074eb5122062ac41fa8b9ed0596d39913b63";
      flake = false;
    };
  };

  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      #
      libclang-bindings-src,
      c-expr-src,
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
        inherit
          nixpkgs
          libclang-bindings-src
          c-expr-src
          doxygen-parser-src
          ;
      };
      flake.overlays = overlays;
    };
}
