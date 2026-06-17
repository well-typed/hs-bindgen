{
  hlib,
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
  # Skip libclang-bindings' own test suite: it is a dependency here (a
  # source-repository-package, whose tests cabal does not run either), and its
  # version sanity check rejects clang versions newer than its pin, which would
  # otherwise break the dev shells on every new LLVM/Clang release.
  libclang-bindings = hlib.dontCheck (
    hlib.addBuildDepends [
      llvmPackages.libclang
      llvmPackages.llvm
    ] libclang-bindings-base
  );
in
libclang-bindings
