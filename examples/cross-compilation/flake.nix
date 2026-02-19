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

        # Target sysroot (glibc headers for cross-compilation)
        aarch64Sysroot = pkgsAarch64.glibc.dev;

        # Cross-compiler binary
        # Note: Nix uses aarch64-unknown-linux-gnu, GNU uses aarch64-linux-gnu
        aarch64Gcc = "${pkgsAarch64.stdenv.cc}/bin/aarch64-unknown-linux-gnu-gcc";

        # Cross-compiled GHC toolchain
        # Runs on x86_64 but produces target-architecture binaries
        ghcAarch64 = pkgsAarch64.buildPackages.ghc;
        cabalAarch64 = pkgsAarch64.buildPackages.cabal-install;

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
              pkgsAarch64.buildPackages.gcc
              ghcAarch64
              cabalAarch64
            ];

            shellHook = ''
              echo "hs-bindgen cross-compilation environment"
              echo "  Target: aarch64-linux-gnu"
              echo "  Run: ./generate-and-run.sh [all|native|aarch64]"
              echo ""

              # Cross-compiled GHC paths (read by generate-and-run.sh)
              export GHC_AARCH64_PATH="${ghcAarch64}/bin/aarch64-unknown-linux-gnu-ghc"
              export CABAL_AARCH64_PATH="${cabalAarch64}/bin/cabal"

              # Target sysroot (headers for cross-compilation)
              export AARCH64_SYSROOT="${aarch64Sysroot}"

              # Cross-compiler path
              export AARCH64_CC="${aarch64Gcc}"

              # QEMU library path (sysroot for runtime library access)
              export QEMU_AARCH64_LD_PREFIX="${aarch64Sysroot}/.."
            '';
          });

          # Native-only (no cross-GHC, fast to evaluate)
          cross-native = pkgs.mkShell (commonShellVars // {
            name = "hs-bindgen-native";
            buildInputs = commonBuildInputs;
          });

          # AArch64 cross-compilation only
          cross-aarch64 = pkgs.mkShell (commonShellVars // {
            name = "hs-bindgen-cross-aarch64";

            buildInputs = commonBuildInputs ++ [
              pkgs.qemu
              pkgsAarch64.buildPackages.gcc
              ghcAarch64
              cabalAarch64
            ];

            shellHook = ''
              export GHC_AARCH64_PATH="${ghcAarch64}/bin/aarch64-unknown-linux-gnu-ghc"
              export CABAL_AARCH64_PATH="${cabalAarch64}/bin/cabal"
              export AARCH64_SYSROOT="${aarch64Sysroot}"
              export AARCH64_CC="${aarch64Gcc}"
              export QEMU_AARCH64_LD_PREFIX="${aarch64Sysroot}/.."
            '';
          });
        };

        # Nix derivations for building the C library for each target
        packages = {
          lib-native = pkgs.stdenv.mkDerivation {
            name = "arch-types-native";
            src = ./c-src;
            buildPhase = "make";
            installPhase = ''
              mkdir -p $out/lib $out/include
              cp libarch_types.* $out/lib/
              cp arch_types.h $out/include/
            '';
          };

          lib-aarch64 = pkgsAarch64.stdenv.mkDerivation {
            name = "arch-types-aarch64";
            src = ./c-src;
            buildPhase = "make";
            installPhase = ''
              mkdir -p $out/lib $out/include
              cp libarch_types.* $out/lib/
              cp arch_types.h $out/include/
            '';
          };
        };
      }
    );
}
