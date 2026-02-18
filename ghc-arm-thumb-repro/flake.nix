{
  description = "Minimal reproducer for GHC ARM Thumb interworking bug (ghc#21991)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        # GHC 9.8.4 cross-compiling to ARM32 (unpatched — triggers the bug)
        pkgsArm32 = pkgs.pkgsCross.armv7l-hf-multiplatform;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.qemu
            pkgsArm32.buildPackages.gcc
            pkgsArm32.buildPackages.ghc
            pkgsArm32.buildPackages.cabal-install
          ];
          shellHook = ''
            export GHC="${pkgsArm32.buildPackages.ghc}/bin/armv7l-unknown-linux-gnueabihf-ghc"
            export CABAL="${pkgsArm32.buildPackages.cabal-install}/bin/cabal"
            export QEMU_ARM_LD_PREFIX="${pkgsArm32.glibc.dev}/.."
          '';
        };
      });
}
