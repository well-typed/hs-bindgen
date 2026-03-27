{
  pkgsDefault,
  pkgsOverlay,
  llvmPackages,
  #
  additionalPackages ? [ ],
  appendToShellHook ? "",
}:

let
  hpkgsDefault = pkgsDefault.haskellPackages;
  hpkgsOverlay = pkgsOverlay.haskellPackages;
  devShellWith = hpkgsOverlay.shellFor {
    packages = p: [ p.hs-bindgen ];
    nativeBuildInputs = [
      # Haskell toolchain.
      hpkgsOverlay.cabal-install
      hpkgsOverlay.haskell-language-server
      # Haskell tools.
      # Fix the version of `cabal-fmt` because it compiles only with some
      # versions of GHC.
      hpkgsDefault.cabal-fmt
      # Clang.
      llvmPackages.clang
      llvmPackages.libclang
      llvmPackages.llvm
      # Doxygen (optional, for extracting structured comments from C headers).
      # Pinned to 1.15.0 — CI workflows install the same version from GitHub
      # releases (see .github/workflows/).  Different doxygen versions produce
      # different XML for edge cases (e.g., file-level comment association),
      # so fixtures must be generated with a consistent version.
      (pkgsDefault.doxygen.overrideAttrs (old: rec {
        version = "1.15.0";
        src = pkgsDefault.fetchurl {
          url = "https://github.com/doxygen/doxygen/releases/download/Release_${builtins.replaceStrings ["."] ["_"] version}/doxygen-${version}.src.tar.gz";
          hash = "sha256-qMr+YFhnrUdaryiKOFJ4MHbh34OqvxZIi7+pWAYudEA=";
        };
      }))
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
    shellHook = import ./hs-bindgen-shell-hook.nix {
      inherit (pkgsDefault) git;
      inherit llvmPackages;
    };
    withHoogle = true;
  };
in
devShellWith
