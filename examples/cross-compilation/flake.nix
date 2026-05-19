{
  description = "Cross-compilation example for hs-bindgen";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Cross-compilation package sets
        #
        # We pin nixos-25.05 (GHC 9.8.4) as a known-good baseline. The pin
        # is conservative for aarch64-only: the historical reason was an
        # LLVM bug in the 32-bit ARM backend
        # (`ARMAsmPrinter::emitXXStructor`, null `Subtarget`) that
        # affected GHC 9.10+ when cross-compiling to ARM32 with LLVM
        # 17-21. Upstream refs: llvm/llvm-project#165422, ghc#26510,
        # NixOS/nixpkgs#466116. ARM32 was removed from this example, so
        # for aarch64 the pin can likely be relaxed to a newer nixpkgs --
        # this just hasn't been re-validated. Bumping the pin would also
        # need to re-check ghc#26937 (GHC runtime linker / TH on ARM32),
        # which is not a concern for aarch64 either.
        pkgsAarch64 = pkgs.pkgsCross.aarch64-multiplatform;

        # Target sysroot (glibc headers + libraries).
        # Used for two purposes:
        #   1. QEMU runtime: provides the dynamic linker and system libraries
        #      (via QEMU_AARCH64_LD_PREFIX, which points to the parent directory)
        #   2. Binding generation: when C headers #include system headers
        #      (e.g. <stdint.h>), hs-bindgen-cli's --target flag goes through
        #      libclang directly (not the CC wrapper), so it may need an
        #      explicit -isystem pointing at these headers
        # Note: the aarch64Clang wrapper below bakes in these headers
        # automatically, so C compilation via `make CC=...` does NOT need
        # this sysroot separately.
        aarch64Sysroot = pkgsAarch64.glibc.dev;

        # Cross-compiling Clang (runs on host, targets aarch64).
        # This is the Nix CC wrapper from the LLVM stdenv -- a Clang binary
        # with target sysroot headers (glibc), linker paths, and the correct
        # --target flag baked in. Using `make CC="$AARCH64_CC"` just works
        # without extra -isystem or --sysroot flags.
        #
        # We use llvmPackages.stdenv.cc (Clang) rather than buildPackages.gcc
        # (GCC) for consistency with hs-bindgen-cli, which is built on
        # libclang/LLVM.
        #
        # Note: pkgsAarch64.clang (without .llvmPackages) is a target-arch
        # package whose wrapper uses aarch64 bash -- it cannot run on x86_64.
        aarch64Clang = "${pkgsAarch64.llvmPackages.stdenv.cc}/bin/aarch64-unknown-linux-gnu-cc";

        # Cross-compiled GHC toolchain
        # Runs on x86_64 but produces target-architecture binaries
        ghcAarch64 = pkgsAarch64.buildPackages.ghc;
        cabalAarch64 = pkgsAarch64.buildPackages.cabal-install;

        # Target-architecture GMP (libgmp).
        # GHC's RTS links against libgmp, and the cross-linker needs the
        # target-architecture version on the library search path when linking
        # the final executable.
        aarch64Gmp = pkgsAarch64.gmp;

        # Target-arch libclang and zlib (TH mode only -- iserv dlopens
        # libclang.so under QEMU; hs-bindgen pulls in zlib transitively).
        # `.dev` exposes the C headers (clang-c/Index.h) that
        # libclang-bindings's configure script needs.
        # See manual/low-level/usage/cross-compilation.md.
        aarch64Libclang    = pkgsAarch64.llvmPackages.libclang.lib;
        aarch64LibclangDev = pkgsAarch64.llvmPackages.libclang.dev;
        aarch64Zlib        = pkgsAarch64.zlib;

        # Common tools needed by all shells
        commonBuildInputs = with pkgs; [
          gnumake
          clang
          llvmPackages.libclang
          llvmPackages.llvm
          ghc
          cabal-install
        ];

        # Common env vars for all shells
        commonShellVars = {
          BINDGEN_EXTRA_CLANG_ARGS = "";
          BINDGEN_BUILTIN_INCLUDE_DIR = "";
          LD_LIBRARY_PATH = "${pkgs.lib.getLib pkgs.llvmPackages.libclang}/lib";
        };

        # Variables consumed by examples/cross-compilation/generate-and-run.sh.
        aarch64Exports = ''
          export GHC_AARCH64_PATH="${ghcAarch64}/bin/aarch64-unknown-linux-gnu-ghc"
          export CABAL_AARCH64_PATH="${cabalAarch64}/bin/cabal"
          export AARCH64_SYSROOT="${aarch64Sysroot}"
          export AARCH64_CC="${aarch64Clang}"
          export AARCH64_GMP_LIB="${aarch64Gmp}/lib"
          export AARCH64_LIBCLANG_LIB="${aarch64Libclang}/lib"
          export AARCH64_LIBCLANG_INCLUDE="${aarch64LibclangDev}/include"
          export AARCH64_ZLIB_LIB="${aarch64Zlib.out}/lib"
          export AARCH64_ZLIB_INCLUDE="${aarch64Zlib.dev}/include"
          export QEMU_AARCH64_LD_PREFIX="${aarch64Sysroot}/.."
        '';

        mkAarch64Shell = { name, banner ? "" }: pkgs.mkShell (commonShellVars // {
          inherit name;
          buildInputs = commonBuildInputs ++ [
            pkgs.qemu
            ghcAarch64
            cabalAarch64
            aarch64Libclang
            aarch64LibclangDev
            aarch64Zlib
          ];
          shellHook = banner + aarch64Exports;
        });

      in
      {
        devShells = {
          default = mkAarch64Shell {
            name = "hs-bindgen-cross-compilation";
            banner = ''
              echo "hs-bindgen cross-compilation environment"
              echo "  Target: aarch64-linux-gnu"
              echo "  Run: ./generate-and-run.sh"
              echo ""
            '';
          };

          cross-aarch64 = mkAarch64Shell {
            name = "hs-bindgen-cross-aarch64";
          };
        };
      }
    );
}
