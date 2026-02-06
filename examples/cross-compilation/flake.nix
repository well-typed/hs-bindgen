{
  description = "Cross-compilation example for hs-bindgen";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Cross-compilation package sets
        pkgsAarch64 = pkgs.pkgsCross.aarch64-multiplatform;
        pkgsArm32 = pkgs.pkgsCross.armv7l-hf-multiplatform;

        # Target sysroots (glibc headers for cross-compilation)
        aarch64Sysroot = pkgsAarch64.glibc.dev;
        arm32Sysroot = pkgsArm32.glibc.dev;

        # Cross-compiler binaries
        # Note: Nix uses aarch64-unknown-linux-gnu, GNU uses aarch64-linux-gnu
        aarch64Gcc = "${pkgsAarch64.stdenv.cc}/bin/aarch64-unknown-linux-gnu-gcc";
        arm32Gcc = "${pkgsArm32.stdenv.cc}/bin/armv7l-unknown-linux-gnueabihf-gcc";

        # Cross-compiled GHC toolchains
        # These run on x86_64 but produce target-architecture binaries
        ghcAarch64 = pkgsAarch64.buildPackages.ghc;
        cabalAarch64 = pkgsAarch64.buildPackages.cabal-install;
        ghcArm32 = pkgsArm32.buildPackages.ghc;
        cabalArm32 = pkgsArm32.buildPackages.cabal-install;

        # Common tools needed by all shells
        commonBuildInputs = with pkgs; [
          gnumake
          clang
          llvmPackages.libclang
          ghc
          cabal-install
        ];

        # Common env vars for all shells
        commonShellVars = {
          BINDGEN_EXTRA_CLANG_ARGS = "";
          BINDGEN_BUILTIN_INCLUDE_DIR = "";
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
              pkgsArm32.buildPackages.gcc
              ghcAarch64
              cabalAarch64
              ghcArm32
              cabalArm32
            ];

            shellHook = ''
              echo "hs-bindgen cross-compilation environment"
              echo "  Targets: aarch64-linux-gnu, arm-linux-gnueabihf"
              echo "  Run: ./generate-and-run.sh [all|native|aarch64|arm32]"
              echo ""

              # Cross-compiled GHC paths (read by generate-and-run.sh)
              export GHC_AARCH64_PATH="${ghcAarch64}/bin/aarch64-unknown-linux-gnu-ghc"
              export CABAL_AARCH64_PATH="${cabalAarch64}/bin/cabal"
              export GHC_ARM32_PATH="${ghcArm32}/bin/armv7l-unknown-linux-gnueabihf-ghc"
              export CABAL_ARM32_PATH="${cabalArm32}/bin/cabal"

              # Target sysroots (headers for cross-compilation)
              export AARCH64_SYSROOT="${aarch64Sysroot}"
              export ARM32_SYSROOT="${arm32Sysroot}"

              # Cross-compiler paths
              export AARCH64_CC="${aarch64Gcc}"
              export ARM32_CC="${arm32Gcc}"

              # QEMU library paths (sysroot for runtime library access)
              export QEMU_AARCH64_LD_PREFIX="${aarch64Sysroot}/.."
              export QEMU_ARM_LD_PREFIX="${arm32Sysroot}/.."
            '';
          });

          # Native-only (no cross-GHC, fast to evaluate)
          cross-native = pkgs.mkShell (commonShellVars // {
            name = "hs-bindgen-native";
            buildInputs = commonBuildInputs;
          });

          # ARM32 cross-compilation only
          cross-arm32 = pkgs.mkShell (commonShellVars // {
            name = "hs-bindgen-cross-arm32";

            buildInputs = commonBuildInputs ++ [
              pkgs.qemu
              pkgsArm32.buildPackages.gcc
              ghcArm32
              cabalArm32
            ];

            shellHook = ''
              export GHC_ARM32_PATH="${ghcArm32}/bin/armv7l-unknown-linux-gnueabihf-ghc"
              export CABAL_ARM32_PATH="${cabalArm32}/bin/cabal"
              export ARM32_SYSROOT="${arm32Sysroot}"
              export ARM32_CC="${arm32Gcc}"
              export QEMU_ARM_LD_PREFIX="${arm32Sysroot}/.."
            '';
          });

          # AArch64 cross-compilation only (not used in CI, available for local use)
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

          lib-arm32 = pkgsArm32.stdenv.mkDerivation {
            name = "arch-types-arm32";
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
