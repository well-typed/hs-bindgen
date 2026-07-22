{
  lib,
  stdenv,
  makeBinaryWrapper,
  #
  haskellPackages,
  llvmPackages,
  # Nixpkgs doxygen (avoids rebuilds for users). Note this may differ from the
  # pinned doxygen used for golden-test fixtures in `hs-bindgen-dev.nix`.
  doxygen,
}:

stdenv.mkDerivation {
  pname = "hs-bindgen-cli";
  inherit (haskellPackages.hs-bindgen) version;

  nativeBuildInputs = [ makeBinaryWrapper ];

  buildCommand = ''
    mkdir -p $out/bin
    cp ${haskellPackages.hs-bindgen}/bin/hs-bindgen-cli $out/bin/hs-bindgen-cli

    # NOTE: Building packages with `hs-bindgen` requires a separate setup hook
    # (doh !) which is defined in `hs-bindgen-hook.sh`. Please keep this wrapper
    # and the setup hook synchronized!
    #
    # `hs-bindgen-cli` shells out at runtime to `clang` (macro reparsing) and
    # `doxygen` (doc comments), both discovered via PATH. The `clang` must be
    # version-matched to the linked `libclang`, so we put the matching
    # `llvmPackages.clang` on PATH here. `--prefix` (not `--suffix`) ensures our
    # matched clang wins; `LLVM_PATH` still overrides discovery entirely.
    wrapProgram $out/bin/hs-bindgen-cli \
      --prefix PATH : ${lib.makeBinPath [ llvmPackages.clang doxygen ]} \
      --set-default BINDGEN_EXTRA_CLANG_ARGS "$(<${llvmPackages.clang}/nix-support/cc-cflags) $(<${llvmPackages.clang}/nix-support/libc-cflags) $NIX_CFLAGS_COMPILE" \
      --set-default BINDGEN_BUILTIN_INCLUDE_DIR disable
  '';

  # Expose the paired toolchain so downstream references the guaranteed match
  # instead of hardcoding an LLVM version.
  passthru = {
    inherit llvmPackages doxygen;
    inherit (llvmPackages) clang;
  };
}
