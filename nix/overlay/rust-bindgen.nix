# NOTE: Override the LLVM version used by `rust-bindgen`, and the version of
# `rust-bindgen` itself which has to align with the one used to create the
# fixtures.
{
  maybeLlvmPackages ? null,
}:

final: prev:
let
  llvmPackages = if maybeLlvmPackages == null then final.llvmPackages else maybeLlvmPackages;
  clang = llvmPackages.clang;
in
{
  rust-bindgen-unwrapped = prev.rust-bindgen-unwrapped.overrideAttrs (old: rec {
    version = "0.71.1";
    src = prev.fetchCrate {
      pname = "bindgen-cli";
      inherit version;
      hash = "sha256-RL9P0dPYWLlEGgGWZuIvyULJfH+c/B+3sySVadJQS3w=";
    };
    cargoDeps = final.pkgs.rustPlatform.fetchCargoVendor {
      pname = old.pname;
      inherit version src;
      hash = "sha256-4EyDjHreFFFSGf7UoftCh6eI/8nfIP1ANlYWq0K8a3I=";
    };
    preConfigure = ''
      export LIBCLANG_PATH="${final.lib.getLib clang.cc}/lib"
    '';
    nativeCheckInputs = [ clang ];
    passthru = {
      clang = clang;
    };
  });
}
