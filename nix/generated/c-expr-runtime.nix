{ mkDerivation, base, containers, data-default, fetchgit, fin, lib
, libclang-bindings, some, template-haskell, text, vec
}:
mkDerivation {
  pname = "c-expr-runtime";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/well-typed/c-expr";
    sha256 = "1fns28r1g4aa6876gwni50ky9f2kwdzgbw7a886xrfc0vslg0rm0";
    rev = "19aeaeb8d58ea6c49ee8cbb7ac741bb981e6f27d";
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
