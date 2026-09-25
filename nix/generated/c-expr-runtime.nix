{ mkDerivation, base, containers, data-default, fetchgit, fin, lib
, libclang-bindings, some, template-haskell, text, vec
}:
mkDerivation {
  pname = "c-expr-runtime";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/well-typed/c-expr";
    sha256 = "1m089b4rv3cmzgfxwv8yg36fb4ra1iy60g8j0gh2mxpcj3cd3daf";
    rev = "c557239af747ac324c84d7022f08931edb890b9e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/c-expr-runtime; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base containers fin some template-haskell vec
  ];
  testHaskellDepends = [
    base containers data-default fin libclang-bindings text vec
  ];
  description = "Haskell DSL for simple C arithmetic expressions";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
