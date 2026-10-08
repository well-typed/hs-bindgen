{ mkDerivation, base, containers, directory, filepath, lib, process
, QuickCheck, tasty, tasty-hunit, tasty-quickcheck, temporary, text
, xml-conduit
}:
mkDerivation {
  pname = "doxygen-parser";
  version = "0.1.2";
  sha256 = "f734d40aacf73ea25b151ba62e68b09549b71a934f506160a5014b770422e6f2";
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
