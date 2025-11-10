{
  nixpkgs,
  libclang-bindings-src,
}:

{
  system,
  lib,
  pkgs,
  ...
}:

let
  ghcs = {
    ghc94 = "ghc94";
    ghc96 = "ghc96";
    ghc98 = "ghc98";
    ghc910 = "ghc910";
    ghc912 = "ghc912";
  };
  llvms = {
    llvm18 = "18";
    llvm19 = "19";
    llvm20 = "20";
    llvm21 = "21";
  };
  pkgsDefault = pkgs;
  pkgsOverlayWith =
    {
      maybeGhc ? null,
      maybeLlvmPackages ? null,
    }:
    import nixpkgs {
      inherit system;
      overlays = [
        (import ./overlay {
          inherit lib libclang-bindings-src maybeLlvmPackages;
        }).default
        (final: prev: {
          haskellPackages =
            if maybeGhc == null then prev.haskellPackages else final.haskell.packages.${maybeGhc};
        })
      ];
    };
  pkgsOverlay = pkgsOverlayWith { };
  devShellWith = import ./hs-bindgen-dev.nix;
  devShells = lib.concatMapAttrs (
    g: ghc:
    lib.concatMapAttrs (
      l: llvmVersion:
      let
        llvmPackages = pkgs."llvmPackages_${llvmVersion}";
        pkgsOverlay = pkgsOverlayWith {
          maybeGhc = ghc;
          maybeLlvmPackages = llvmPackages;
        };
      in
      {
        "${g}-${l}" = devShellWith { inherit pkgsDefault pkgsOverlay llvmPackages; };
      }
    ) llvms
  ) ghcs;
in
{
  packages = {
    inherit (pkgsOverlay) hsBindgenHook hs-bindgen-cli;
    default = pkgsOverlay.hs-bindgen-cli;
  };

  devShells = devShells // {
    default = devShellWith {
      inherit pkgsDefault pkgsOverlay;
      inherit (pkgsDefault) llvmPackages;
    };
  };
}
