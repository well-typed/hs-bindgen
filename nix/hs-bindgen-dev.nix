{
  pkgs,
}:

let
  lib = pkgs.lib;
  hpkgs = pkgs.haskell.packages;
  ghcs = {
    inherit (hpkgs)
      ghc94
      ghc96
      ghc98
      ghc910
      ghc912
      ;
  };
  llvms = {
    llvm18 = pkgs.llvmPackages_18;
    llvm19 = pkgs.llvmPackages_19;
    llvm20 = pkgs.llvmPackages_20;
    llvm21 = pkgs.llvmPackages_21;
  };
  devShellWith =
    {
      haskellPackages ? pkgs.haskellPackages,
      llvmPackages ? pkgs.llvmPackages,
      additionalPackages ? [ ],
      appendToShellHook ? "",
    }:
    haskellPackages.shellFor {
      packages = p: [ p.hs-bindgen ];
      nativeBuildInputs = [
        # Haskell.
        haskellPackages.cabal-install
        haskellPackages.ghc
        haskellPackages.haskell-language-server
        # Rust.
        pkgs.rust-bindgen
        pkgs.rustfmt
        # Clang.
        llvmPackages.clang
        llvmPackages.libclang
        llvmPackages.llvm
        # Bindgen hook.
        #
        # NOTE: `hsBindgenHook` collects all library dependencies in the
        # closure and adds their `CFLAGS` and `CCFLAGS` to
        # `BINDGEN_EXTRA_CLANG_ARGS`. Since we have GCC in the closure
        # (and not only Clang), the GCC includes end up in
        # BINDGEN_EXTRA_CLANG_ARGS which is suboptimal. We could use a
        # `clangStdenv` Nixpkgs overlay, but that requires recompilation
        # of the complete toolchain; see, e.g.,
        # https://nixos.wiki/wiki/Using_Clang_instead_of_GCC.
        pkgs.hsBindgenHook
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
  devShells = lib.concatMapAttrs (
    h: hpkgs:
    lib.concatMapAttrs (l: lpkgs: {
      "${h}-${l}" = devShellWith {
        haskellPackages = hpkgs;
        llvmPackages = lpkgs;
      };
    }) llvms
  ) ghcs;
in
{
  inherit devShells devShellWith;
}
