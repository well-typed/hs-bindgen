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
  default = lib.composeManyExtensions [
    libclangBindings
    hsBindgen
    hsFixes
  ];
in
{
  inherit
    libclangBindings
    hsBindgen
    hsFixes
    default
    ;
}
