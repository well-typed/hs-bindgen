final: prev: {
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      hfinal: hprev: {
        doxygen-parser = hfinal.callPackage ../generated/doxygen-parser.nix { };
      }
    );
  };
}
