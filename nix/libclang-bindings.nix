{
  haskellLib,
  libclang-bindings-src,
}:

{
  callCabal2nix,
  llvmPackages,
}:
let
  libclang-bindings-base =
    callCabal2nix "libclang-bindings" "${libclang-bindings-src}/libclang-bindings"
      { };
  libclang-bindings = haskellLib.compose.addBuildDepends [
    llvmPackages.libclang
    llvmPackages.llvm
  ] libclang-bindings-base;
in
libclang-bindings
