{
  lib,
  libclang-bindings-src,
  maybeLlvmPackages ? null,
}:
let
  libclangBindings = import ./libclang-bindings.nix {
    inherit libclang-bindings-src maybeLlvmPackages;
  };
  hsBindgen = import ./hs-bindgen.nix { inherit maybeLlvmPackages; };
  hsFixes = import ./overrides.nix;
  rustBindgen = import ./rust-bindgen.nix { inherit maybeLlvmPackages; };
  default = lib.composeManyExtensions [
    libclangBindings
    hsBindgen
    hsFixes
    rustBindgen
  ];
in
{
  inherit
    libclangBindings
    hsBindgen
    hsFixes
    rustBindgen
    default
    ;
}
