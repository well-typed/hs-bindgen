{
  lib,
  libclang-bindings-src,
  c-expr-src,
  doxygen-parser-src,
  maybeLlvmPackages ? null,
}:
let
  libclangBindings = import ./libclang-bindings.nix {
    inherit libclang-bindings-src maybeLlvmPackages;
  };
  c-expr = import ./c-expr.nix {
    inherit c-expr-src;
  };
  doxygenParser = import ./doxygen-parser.nix {
    inherit doxygen-parser-src;
  };
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
