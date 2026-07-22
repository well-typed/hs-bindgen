{ mkDerivation, base, bytestring, c-expr-runtime, containers
, debruijn, filepath, fin, indexed-traversable, lib
, libclang-bindings, mtl, parsec, scientific, some, tasty
, tasty-golden, tasty-hunit, text, vec
}:
mkDerivation {
  pname = "c-expr-dsl";
  version = "0.1.0.1";
  sha256 = "87789fe7531880ac4fee53dfb6ea13ae6cdbcc7ab75265610bb6732e7a7a6de5";
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
