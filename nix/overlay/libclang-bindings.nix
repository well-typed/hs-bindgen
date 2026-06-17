{
  libclang-bindings-src,
  maybeLlvmPackages ? null,
}:

final: prev:
let
  hlib = final.haskell.lib.compose;
  llvmPackages = if maybeLlvmPackages == null then final.llvmPackages else maybeLlvmPackages;
  libclang-bindings = import ../extern/libclang-bindings.nix {
    inherit hlib;
    inherit libclang-bindings-src;
  };
in
{
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      hfinal: hprev: {
        libclang-bindings = hfinal.callPackage libclang-bindings { inherit llvmPackages; };
      }
    );
  };
}
