final: prev:
let
  inherit (final) lib;
  hlib = (final.haskell.lib.compose);
  hsBindgenPkgNames = [
    "ansi-diff"
    "c-expr-dsl"
    "c-expr-runtime"
    "hs-bindgen"
    "hs-bindgen-runtime"
    "hs-bindgen-test-runtime"
  ];
  mkPkg = hpkgs: name: hpkgs.callCabal2nix name (./../../${name}) { };
  mkHsBindgenPkgs = hpkgs: lib.genAttrs hsBindgenPkgNames (mkPkg hpkgs);
in
{
  haskell = prev.haskell // {
    packageOverrides =
      hfinal: hprev:
      let
        hsBindgenPkgs = mkHsBindgenPkgs hfinal;
      in
      prev.haskell.packageOverrides hfinal hprev
      // {
        inherit (hsBindgenPkgs) ansi-diff hs-bindgen-runtime hs-bindgen-test-runtime;
        # TODO: The documentation fails to build.
        c-expr-dsl = hlib.dontHaddock hsBindgenPkgs.c-expr-dsl;
        # TODO: The test of `c-expr-runtime` requires `musl` headers, but
        # providing `musl` as test system dependency causes other build errors.
        c-expr-runtime = hlib.dontCheck hsBindgenPkgs.c-expr-runtime;
        hs-bindgen = hlib.addBuildDepends [ final.hsBindgenHook ] (
          hlib.addTestToolDepends [
            final.rust-bindgen
          ] hsBindgenPkgs.hs-bindgen
        );
      };
    lib.compose = prev.haskell.lib.compose // final.callPackage ./../hs-bindgen-lib.nix { };
  };
  hsBindgenHook = final.callPackage ./../hs-bindgen-hook.nix { };
  hs-bindgen-cli = final.callPackage ./../hs-bindgen-cli.nix { };
}
