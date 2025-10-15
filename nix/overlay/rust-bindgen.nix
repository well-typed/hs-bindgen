# NOTE: May be used to overwrite the version of `rust-bindgen` which has
# to align with the one used to create the fixtures.
final: prev: {
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
  });
}
