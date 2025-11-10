{
  libclang-bindings-src,
  maybeLlvmPackages ? null,
}:

final: prev:
let
  llvmPackages = if maybeLlvmPackages == null then final.llvmPackages else maybeLlvmPackages;
  libclang-bindings = import ../libclang-bindings.nix {
    haskellLib = final.haskell.lib;
    inherit libclang-bindings-src;
  };
in
{
  haskell = prev.haskell // {
    packageOverrides =
      hfinal: hprev:
      prev.haskell.packageOverrides hfinal hprev
      // {
        libclang-bindings = hfinal.callPackage libclang-bindings { inherit llvmPackages; };
      };
  };
}
