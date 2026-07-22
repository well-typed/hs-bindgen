{
  maybeLlvmPackages ? null,
}:

final: prev:
let
  hlib = final.haskell.lib.compose;
  llvmPackages = if maybeLlvmPackages == null then final.llvmPackages else maybeLlvmPackages;
in
{
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (
      hfinal: hprev: {
        libclang-bindings =
          let
            # `.override` swaps tasty just for this package's closure, so only
            # tasty, its reverse deps below, and libclang-bindings rebuild —
            # not everything else that depends on tasty. tasty-hunit and
            # tasty-quickcheck must be rebuilt against it too, since GHC
            # requires a single tasty instance across the whole closure. Also
            # pinned in nix/generate.sh (HACKAGE_PACKAGES), keep both in sync.
            tasty' = hfinal.callPackage ../generated/tasty.nix { };
            tasty-hunit' = hprev.tasty-hunit.override { tasty = tasty'; };
            tasty-quickcheck' = hprev.tasty-quickcheck.override { tasty = tasty'; };
          in
          hlib.addBuildDepends [
            llvmPackages.libclang
            llvmPackages.llvm
          ] (
            (hfinal.callPackage ../generated/libclang-bindings.nix { }).override {
              tasty = tasty';
              tasty-hunit = tasty-hunit';
              tasty-quickcheck = tasty-quickcheck';
            }
          );
      }
    );
  };
}
