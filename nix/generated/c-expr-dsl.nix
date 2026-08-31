{ mkDerivation, base, bytestring, c-expr-runtime, containers
, debruijn, fetchgit, filepath, fin, indexed-traversable, lib
, libclang-bindings, mtl, parsec, scientific, some, tasty
, tasty-golden, tasty-hunit, text, vec
}:
mkDerivation {
  pname = "c-expr-dsl";
  version = "0.1.0.1";
  src = fetchgit {
    url = "https://github.com/well-typed/c-expr";
    sha256 = "04nirf7sh0lghmgr0my53iji0br3kjrcqdi5fwlnwyr44lrm9i6p";
    rev = "ca353fb216ee971396678feac277ce3c7015bec6";
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
