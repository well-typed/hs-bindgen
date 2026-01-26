{
  haskellLib,
  libclang-bindings-src,
}:

{
  callCabal2nix,
  llvmPackages,
}:
let
  libclang-bindings-base = callCabal2nix "libclang-bindings" "${libclang-bindings-src}" {
    # Truly override the LLVM version used. Use `cabal2nix` and drectly inspect
    # the derivation created by `callCabal2nix`.
    inherit (llvmPackages) libclang;
  };
  libclang-bindings = haskellLib.compose.addBuildDepends [
    llvmPackages.libclang
    llvmPackages.llvm
  ] libclang-bindings-base;
in
libclang-bindings
