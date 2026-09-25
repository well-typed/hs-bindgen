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
    sha256 = "0bpymkznrnsk664l23qsds0nfxx6g7cmh1h2a5lirz8wd4wx6hb0";
    rev = "8ca96da305770f76df10c9f9a09aa1d56ae9a46e";
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
