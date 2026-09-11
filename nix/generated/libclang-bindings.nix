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
    sha256 = "0aq6d0snfbgwa1ayjfwlhck5za528fm7gnywmjc4185a2xspdfy2";
    rev = "7c5fbd726aa0ac02ef021f257c93d2f98fb54b27";
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
