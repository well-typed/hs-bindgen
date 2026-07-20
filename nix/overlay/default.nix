{
  lib,
  maybeLlvmPackages ? null,
}:
let
  libclangBindings = import ./libclang-bindings.nix {
    inherit maybeLlvmPackages;
  };
  c-expr = import ./c-expr.nix;
  doxygenParser = import ./doxygen-parser.nix;
  hsBindgen = import ./hs-bindgen.nix { inherit maybeLlvmPackages; };
  hsFixes = import ./overrides.nix;
  default = lib.composeManyExtensions [
    libclangBindings
    c-expr
    doxygenParser
    hsBindgen
    hsFixes
  ];
in
{
  inherit
    libclangBindings
    c-expr
    doxygenParser
    hsBindgen
    hsFixes
    default
    ;
}
