# NOTE: Provide a specific version of `rust-bindgen` with a specific LLVM
# version that aligns with the one used to produce the fixtures.
{
  lib,
  fetchCrate,
  rustPlatform,
  #
  clang,
  rust-bindgen,
  rust-bindgen-unwrapped,
}:

let
  rust-bindgen-unwrapped-pinned = rust-bindgen-unwrapped.overrideAttrs (old: rec {
    version = "0.71.1";
    src = fetchCrate {
      pname = "bindgen-cli";
      inherit version;
      hash = "sha256-RL9P0dPYWLlEGgGWZuIvyULJfH+c/B+3sySVadJQS3w=";
    };
    cargoDeps = rustPlatform.fetchCargoVendor {
      pname = old.pname;
      inherit version src;
      hash = "sha256-4EyDjHreFFFSGf7UoftCh6eI/8nfIP1ANlYWq0K8a3I=";
    };
    preConfigure = ''
      export LIBCLANG_PATH="${lib.getLib clang.cc}/lib"
    '';
    nativeCheckInputs = [ clang ];
    passthru = {
      clang = clang;
    };
  });
in
rust-bindgen.override (old: {
  rust-bindgen-unwrapped = rust-bindgen-unwrapped-pinned;
})
