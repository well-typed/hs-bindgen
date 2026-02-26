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
        # NOTE: GHC 9.10+ cannot cross-compile to ARM32 due to an LLVM bug:
        #   llc segfaults in ARMAsmPrinter::emitXXStructor (null Subtarget)
        #   Affects all LLVM versions 17–21. Fixed on LLVM main (PR #166329),
        #   but the backport to release/21.x was rejected (#168380).
        #   Upstream issues: llvm/llvm-project#165422, ghc#26510,
        #   NixOS/nixpkgs#466116
        # GHC 9.8 generates different IR that does not trigger this bug,
        # so we pin to nixos-25.05 (GHC 9.8.4) until LLVM 22 ships.
        #
        # ARM32 cross-compilation with Template Haskell is additionally
        # blocked by a GHC runtime linker bug (ghc#26937).
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

      in
      {
        devShells = {
          # Full environment with all cross-targets (for local development)
          default = pkgs.mkShell (commonShellVars // {
            name = "hs-bindgen-cross-compilation";

            buildInputs = commonBuildInputs ++ [
              pkgs.qemu
              ghcAarch64
              cabalAarch64
            ];

            shellHook = ''
              echo "hs-bindgen cross-compilation environment"
              echo "  Target: aarch64-linux-gnu"
              echo "  Run: ./generate-and-run.sh"
              echo ""

              # Cross-compiled GHC paths (read by generate-and-run.sh)
              export GHC_AARCH64_PATH="${ghcAarch64}/bin/aarch64-unknown-linux-gnu-ghc"
              export CABAL_AARCH64_PATH="${cabalAarch64}/bin/cabal"

              # Target sysroot: used for QEMU runtime (dynamic linker +
              # system libs) and for hs-bindgen-cli binding generation when
              # C headers #include system headers (via -isystem)
              export AARCH64_SYSROOT="${aarch64Sysroot}"

              # Cross-compiling Clang wrapper (for C library cross-compilation)
              export AARCH64_CC="${aarch64Clang}"

              # Target-arch GMP (GHC's RTS links against libgmp)
              export AARCH64_GMP_LIB="${aarch64Gmp}/lib"

              # QEMU library path (sysroot for runtime library access)
              export QEMU_AARCH64_LD_PREFIX="${aarch64Sysroot}/.."
            '';
          });

          # AArch64 cross-compilation only
          cross-aarch64 = pkgs.mkShell (commonShellVars // {
            name = "hs-bindgen-cross-aarch64";

            buildInputs = commonBuildInputs ++ [
              pkgs.qemu
              ghcAarch64
              cabalAarch64
            ];

            shellHook = ''
              export GHC_AARCH64_PATH="${ghcAarch64}/bin/aarch64-unknown-linux-gnu-ghc"
              export CABAL_AARCH64_PATH="${cabalAarch64}/bin/cabal"
              export AARCH64_SYSROOT="${aarch64Sysroot}"
              export AARCH64_CC="${aarch64Clang}"
              export AARCH64_GMP_LIB="${aarch64Gmp}/lib"
              export QEMU_AARCH64_LD_PREFIX="${aarch64Sysroot}/.."
            '';
          });
        };
      }
    );
}
