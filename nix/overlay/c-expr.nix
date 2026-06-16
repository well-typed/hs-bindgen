{
  c-expr-src,
}:

final: prev:
let
  c-expr = import ../extern/c-expr.nix {
    inherit c-expr-src;
  };
in
{
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      hfinal: hprev: {
        c-expr-dsl = hfinal.callPackage c-expr.c-expr-dsl { };
        c-expr-runtime = hfinal.callPackage c-expr.c-expr-runtime { };
      }
    );
  };
}
