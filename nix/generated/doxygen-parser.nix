{ mkDerivation, base, containers, directory, filepath, lib, process
, QuickCheck, tasty, tasty-hunit, tasty-quickcheck, temporary, text
, xml-conduit
}:
mkDerivation {
  pname = "doxygen-parser";
  version = "0.1.1";
  sha256 = "ed03f98e3d655427a298df7b8cbedb7d3e1342a9467b8c75d1bd9f57d17a833c";
  libraryHaskellDepends = [
    base containers directory filepath process temporary text
    xml-conduit
  ];
  testHaskellDepends = [
    base containers QuickCheck tasty tasty-hunit tasty-quickcheck text
    xml-conduit
  ];
  doHaddock = false;
  homepage = "https://github.com/well-typed/doxygen-parser";
  description = "Parse Doxygen XML output into a typed Haskell AST";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
