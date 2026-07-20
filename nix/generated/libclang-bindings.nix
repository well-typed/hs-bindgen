{ mkDerivation, base, bytestring, containers, data-default
, directory, exceptions, filepath, lib, mtl, process, QuickCheck
, tasty, tasty-hunit, tasty-quickcheck, template-haskell, text
, transformers, unliftio-core
}:
mkDerivation {
  pname = "libclang-bindings";
  version = "0.1.0.0";
  sha256 = "3c4b9c61c78c99b4cf1362cdf18247f933f6493ce23ace23ffec8132ee998db6";
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
