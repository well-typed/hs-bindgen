{ mkDerivation, base, containers, data-default, fin, lib
, libclang-bindings, some, template-haskell, text, vec
}:
mkDerivation {
  pname = "c-expr-runtime";
  version = "0.1.0.0";
  sha256 = "827b0a340f914f2d627e09852fca87df3092c691179ae9db039415772aad7d35";
  libraryHaskellDepends = [
    base containers fin some template-haskell vec
  ];
  testHaskellDepends = [
    base containers data-default fin libclang-bindings text vec
  ];
  description = "Haskell DSL for simple C arithmetic expressions";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
