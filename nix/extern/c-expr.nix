{
  c-expr-src,
}:

{
  c-expr-dsl =
    {
      callCabal2nix,
    }:
    callCabal2nix "c-expr-dsl" "${c-expr-src}/c-expr-dsl" { };

  c-expr-runtime =
    {
      callCabal2nix,
    }:
    callCabal2nix "c-expr-runtime" "${c-expr-src}/c-expr-runtime" { };
}
