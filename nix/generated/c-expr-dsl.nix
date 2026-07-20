{ mkDerivation, base, bytestring, c-expr-runtime, containers
, debruijn, fetchgit, filepath, fin, indexed-traversable, lib
, libclang-bindings, mtl, parsec, scientific, some, tasty
, tasty-golden, tasty-hunit, text, vec
}:
mkDerivation {
  pname = "c-expr-dsl";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/well-typed/c-expr";
    sha256 = "1fns28r1g4aa6876gwni50ky9f2kwdzgbw7a886xrfc0vslg0rm0";
    rev = "19aeaeb8d58ea6c49ee8cbb7ac741bb981e6f27d";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/c-expr-dsl; echo source root reset to $sourceRoot";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring c-expr-runtime containers debruijn fin
    indexed-traversable libclang-bindings mtl parsec scientific some
    text vec
  ];
  testHaskellDepends = [
    base bytestring c-expr-runtime containers debruijn filepath fin
    libclang-bindings parsec tasty tasty-golden tasty-hunit text vec
  ];
  description = "DSL for the language support by c-expr-runtime";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
