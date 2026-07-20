{ mkDerivation, aeson, ansi-terminal, array, async, base
, base-compat, base16-bytestring, bytestring, c-expr-dsl
, c-expr-runtime, containers, contra-tracer, cryptohash-sha256
, data-default, debruijn, deepseq, Diff, directory, doxygen-parser
, edit-distance, exceptions, filepath, fin, hs-bindgen-runtime
, language-c, lib, libclang-bindings, mtl, optics, optics-core
, optparse-applicative, parsec, pretty, prettyprinter, primitive
, process, regex-pcre-builtin, some, syb, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, text, transformers
, unliftio-core, utf8-string, vec, vector, witherable, yaml
}:
mkDerivation {
  pname = "hs-bindgen";
  version = "0.1.0";
  src = ../../hs-bindgen;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal array base base-compat base16-bytestring
    bytestring c-expr-dsl c-expr-runtime containers contra-tracer
    cryptohash-sha256 data-default debruijn deepseq Diff directory
    doxygen-parser edit-distance exceptions filepath fin
    hs-bindgen-runtime language-c libclang-bindings mtl optics-core
    parsec pretty primitive process regex-pcre-builtin some tasty
    tasty-hunit template-haskell temporary text transformers
    unliftio-core utf8-string vec witherable yaml
  ];
  executableHaskellDepends = [
    base bytestring containers data-default libclang-bindings
    optparse-applicative prettyprinter text
  ];
  testHaskellDepends = [
    async base c-expr-runtime containers data-default directory
    filepath hs-bindgen-runtime libclang-bindings mtl optics
    optics-core parsec process syb tasty tasty-hunit tasty-quickcheck
    template-haskell temporary text utf8-string vector
  ];
  doHaddock = false;
  description = "Generate Haskell bindings from C headers";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "hs-bindgen-cli";
}
