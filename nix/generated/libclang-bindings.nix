{ mkDerivation, base, bytestring, containers, data-default
, directory, exceptions, filepath, lib, mtl, process, QuickCheck
, tasty, tasty-hunit, tasty-quickcheck, template-haskell, text
, transformers, unliftio-core
}:
mkDerivation {
  pname = "libclang-bindings";
  version = "0.2.0.0";
  sha256 = "d93a2c16fc545f09af43b26ae6ac26294462b81b8808d749fa4d4288c0648521";
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
