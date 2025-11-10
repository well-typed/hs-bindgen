{
  pkgsDefault,
  pkgsOverlay,
  llvmPackages,
  #
  additionalPackages ? [ ],
  appendToShellHook ? "",
}:

let
  devShellWith =
    let
      hpkgsDefault = pkgsDefault.haskellPackages;
      hpkgsOverlay = pkgsOverlay.haskellPackages;
    in
    hpkgsOverlay.shellFor {
      packages = p: [ p.hs-bindgen ];
      nativeBuildInputs = [
        # Haskell toolchain.
        hpkgsOverlay.cabal-install
        hpkgsOverlay.ghc
        hpkgsOverlay.haskell-language-server
        # Haskell tools.
        # Fix the version of `cabal-fmt` because it compiles only with some
        # versions of GHC.
        hpkgsDefault.cabal-fmt
        # Rust.
        pkgsOverlay.rust-bindgen
        pkgsDefault.rustfmt
        # Clang.
        llvmPackages.clang
        llvmPackages.libclang
        llvmPackages.llvm
        # Bindgen hook. NOTE: `hsBindgenHook` collects all library dependencies
        # in the closure and adds their `CFLAGS` and `CCFLAGS` to
        # `BINDGEN_EXTRA_CLANG_ARGS`. Since we have GCC in the closure (and not
        # only Clang), the GCC includes end up in BINDGEN_EXTRA_CLANG_ARGS which
        # is suboptimal. We could use a `clangStdenv` Nixpkgs overlay, but that
        # requires recompilation of the complete toolchain; see, e.g.,
        # https://nixos.wiki/wiki/Using_Clang_instead_of_GCC.
        pkgsOverlay.hsBindgenHook
      ]
      ++
        # Additional packages (e.g., of example libraries to generate
        # bindings for).
        additionalPackages;
      shellHook = ''
        PROJECT_ROOT=$(git rev-parse --show-toplevel)
        export PROJECT_ROOT

        # TODO: Adding `libclang` to the linker library path still seems to be
        # necessary, because otherwise Template Haskell issues a warning that it
        # cannot find `libclang.so`.
        LD_LIBRARY_PATH="$PROJECT_ROOT/manual/c:${llvmPackages.libclang.lib}/lib''${LD_LIBRARY_PATH:+:''${LD_LIBRARY_PATH}}"
        export LD_LIBRARY_PATH
      ''
      + appendToShellHook;
      withHoogle = true;
    };
in
devShellWith
