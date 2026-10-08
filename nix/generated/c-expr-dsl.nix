{ mkDerivation, base, bytestring, c-expr-runtime, containers
, debruijn, filepath, fin, indexed-traversable, lib
, libclang-bindings, mtl, parsec, scientific, some, tasty
, tasty-golden, tasty-hunit, text, vec
}:
mkDerivation {
  pname = "c-expr-dsl";
  version = "0.2.0.0";
  sha256 = "b145a82c8b6e48f641212d118524a1bfc83b40f2b842899f4baf8cd90eb36619";
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
