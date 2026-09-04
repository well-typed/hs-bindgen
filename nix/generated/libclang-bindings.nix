{ mkDerivation, base, bytestring, containers, data-default
, directory, exceptions, fetchgit, filepath, lib, mtl, process
, QuickCheck, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, transformers, unliftio-core
}:
mkDerivation {
  pname = "libclang-bindings";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/well-typed/libclang-bindings";
    sha256 = "143igqi3x6l619inc8qnh5849w25vg94af83zb4mhx52832mc5rc";
    rev = "5a2e2532fb550b5a80400453b0f0592be509dd48";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libclang-bindings; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring data-default directory exceptions filepath process
    template-haskell text transformers unliftio-core
  ];
  testHaskellDepends = [
    base containers data-default directory mtl QuickCheck tasty
    tasty-hunit tasty-quickcheck text
  ];
  homepage = "https://github.com/well-typed/libclang-bindings";
  description = "libclang bindings";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
