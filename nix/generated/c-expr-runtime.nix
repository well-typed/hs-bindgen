{ mkDerivation, base, containers, data-default, fin, lib
, libclang-bindings, some, template-haskell, text, vec
}:
mkDerivation {
  pname = "c-expr-runtime";
  version = "0.2.0.0";
  sha256 = "831f276a37cca89171b9cebf6352f1038451bd4a68ee91f4c236b0510e2e6985";
  libraryHaskellDepends = [
    base containers fin some template-haskell vec
  ];
  testHaskellDepends = [
    base containers data-default fin libclang-bindings text vec
  ];
  description = "Haskell DSL for simple C arithmetic expressions";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
