final: prev: {
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      hfinal: hprev: {
        c-expr-dsl = hfinal.callPackage ../generated/c-expr-dsl.nix { };
        c-expr-runtime = hfinal.callPackage ../generated/c-expr-runtime.nix { };
      }
    );
  };
}
