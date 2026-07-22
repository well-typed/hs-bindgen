{ mkDerivation, ansi-terminal, base, containers, lib
, optparse-applicative, stm, tagged, transformers, unix
}:
mkDerivation {
  pname = "tasty";
  version = "1.5.3";
  sha256 = "54a0c7b644813af871a3726ac8771b5e17b5158c792a7acf8f9e2d3ae9360780";
  revision = "2";
  editedCabalFile = "04llcf1i3gawdik0bjhxdgls2wkiqlx0gi76nfh784nv2qzxlpbb";
  libraryHaskellDepends = [
    ansi-terminal base containers optparse-applicative stm tagged
    transformers unix
  ];
  homepage = "https://github.com/UnkindPartition/tasty";
  description = "Modern and extensible testing framework";
  license = lib.licenses.mit;
}
