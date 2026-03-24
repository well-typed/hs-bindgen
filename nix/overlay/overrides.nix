final: prev:
let
  hlib = final.haskell.lib.compose;
in
{
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      hfinal: hprev: {
        # TODO: See https://gitlab.haskell.org/ghc/ghc/-/issues/25681.
        optics = hlib.dontCheck hprev.optics;
      }
    );
  };
}
