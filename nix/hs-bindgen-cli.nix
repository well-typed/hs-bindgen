{
  stdenv,
  makeBinaryWrapper,
  #
  haskellPackages,
  llvmPackages,
}:

stdenv.mkDerivation {
  pname = "hs-bindgen-cli";
  version = haskellPackages.hs-bindgen.version;

  nativeBuildInputs = [ makeBinaryWrapper ];

  buildCommand = ''
    mkdir -p $out/bin
    cp ${haskellPackages.hs-bindgen}/bin/hs-bindgen-cli $out/bin/hs-bindgen-cli

    # NOTE: Building packages with `hs-bindgen` requires a separate setup hook
    # (doh !) which is defined in `hs-bindgen-hook.sh`. Please keep this wrapper
    # and the setup hook synchronized!
    wrapProgram $out/bin/hs-bindgen-cli \
      --set-default BINDGEN_EXTRA_CLANG_ARGS "$(<${llvmPackages.clang}/nix-support/cc-cflags) $(<${llvmPackages.clang}/nix-support/libc-cflags) $NIX_CFLAGS_COMPILE" \
      --set-default BINDGEN_BUILTIN_INCLUDE_DIR disable
  '';
}
