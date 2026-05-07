{
  doxygen-parser-src,
}:

{
  callCabal2nix,
}:
callCabal2nix "doxygen-parser" "${doxygen-parser-src}" { }
