{
  nixpkgs,
  overlays,
}:

{ system, pkgs', ... }:

let
  pkgsHs = import nixpkgs {
    inherit system;
    overlays = [ overlays.default ];
  };
  hsBindgenDev = import ./hs-bindgen-dev.nix { pkgs = pkgsHs; };
in
{
  packages = {
    inherit (pkgsHs) hsBindgenHook hs-bindgen-cli;
  };

  devShells = hsBindgenDev.devShells // {
    default = pkgsHs.callPackage hsBindgenDev.devShellWith { };
    pcap = hsBindgenDev.devShellWith {
      haskellPackages = pkgsHs.haskell.packages.ghc912;
      llvmPackages = pkgsHs.llvmPackages;
      additionalPackages = [
        pkgs'.libpcap
      ];
    };
    wlroots = hsBindgenDev.devShellWith {
      haskellPackages = pkgsHs.haskell.packages.ghc912;
      llvmPackages = pkgsHs.llvmPackages;
      additionalPackages = [
        pkgs'.pixman
        pkgs'.wayland
        pkgs'.wlroots
      ];
      appendToShellHook = ''
        BINDGEN_EXTRA_CLANG_ARGS="-isystem ${pkgs'.wlroots}/include/wlroots-0.19 ''${BINDGEN_EXTRA_CLANG_ARGS}"
      '';
    };
  };

}
