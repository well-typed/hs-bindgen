{
  doxygen-parser-src,
}:

final: prev:
let
  doxygen-parser = import ../doxygen-parser.nix {
    inherit doxygen-parser-src;
  };
in
{
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      hfinal: hprev: {
        doxygen-parser = hfinal.callPackage doxygen-parser { };
      }
    );
  };
}
