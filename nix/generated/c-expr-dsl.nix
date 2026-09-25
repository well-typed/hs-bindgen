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
    sha256 = "1m089b4rv3cmzgfxwv8yg36fb4ra1iy60g8j0gh2mxpcj3cd3daf";
    rev = "c557239af747ac324c84d7022f08931edb890b9e";
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
