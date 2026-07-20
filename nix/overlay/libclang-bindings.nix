{
  maybeLlvmPackages ? null,
}:

final: prev:
let
  hlib = final.haskell.lib.compose;
  llvmPackages = if maybeLlvmPackages == null then final.llvmPackages else maybeLlvmPackages;
in
{
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      hfinal: hprev: {
        libclang-bindings = hlib.addBuildDepends [
          llvmPackages.libclang
          llvmPackages.llvm
        ] (hfinal.callPackage ../generated/libclang-bindings.nix { });
      }
    );
  };
}
