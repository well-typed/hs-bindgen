{
  maybeLlvmPackages ? null,
}:

final: prev:
let
  inherit (final) lib;
  llvmPackages = if maybeLlvmPackages == null then final.llvmPackages else maybeLlvmPackages;
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
  rust-bindgen = final.callPackage ../rust-bindgen.nix { inherit (llvmPackages) clang; };
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
        # TODO: Test of `hs-bindgen` fails because it run `hs-bindgen-cli` which
        # is a build output of `hs-bindgen` (chicken-egg problem).
        hs-bindgen = hlib.overrideCabal (drv: {
          # Tests depend on executable.
          preCheck = ''
            export PATH="$PWD/dist/build/hs-bindgen-cli:$PATH"
          ''
          + (drv.preCheck or "");
          buildDepends = drv.buildDepends or [ ] ++ [
            final.hsBindgenHook
          ];
          testToolDepends = drv.testToolDepends or [ ] ++ [
            rust-bindgen
          ];
        }) hsBindgenPkgs.hs-bindgen;
      };
    lib.compose = prev.haskell.lib.compose // final.callPackage ../hs-bindgen-lib.nix { };
  };
  hsBindgenHook = final.callPackage ../hs-bindgen-hook.nix { inherit llvmPackages; };
  hs-bindgen-cli = final.callPackage ../hs-bindgen-cli.nix { inherit llvmPackages; };
}
