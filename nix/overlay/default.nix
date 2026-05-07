{
  lib,
  libclang-bindings-src,
  doxygen-parser-src,
  maybeLlvmPackages ? null,
}:
let
  libclangBindings = import ./libclang-bindings.nix {
    inherit libclang-bindings-src maybeLlvmPackages;
  };
  doxygenParser = import ./doxygen-parser.nix {
    inherit doxygen-parser-src;
  };
  hsBindgen = import ./hs-bindgen.nix { inherit maybeLlvmPackages; };
  hsFixes = import ./overrides.nix;
  default = lib.composeManyExtensions [
    libclangBindings
    doxygenParser
    hsBindgen
    hsFixes
  ];
in
{
  inherit
    libclangBindings
    doxygenParser
    hsBindgen
    hsFixes
    default
    ;
}
