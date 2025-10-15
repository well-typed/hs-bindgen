{
  libclang-bindings-src,
}:

final: prev:
let
  hlib = final.haskell.lib.compose;
  inherit (final.llvmPackages) libclang llvm;
in
{
  haskell = prev.haskell // {
    packageOverrides =
      hfinal: hprev:
      let
        libclang-bindings = hfinal.callCabal2nix "libclang-bindings" "${libclang-bindings-src}" { };
      in
      prev.haskell.packageOverrides hfinal hprev
      // {
        libclang-bindings = hlib.addBuildDepends [
          libclang
          llvm
        ] libclang-bindings;
      };
  };
}
