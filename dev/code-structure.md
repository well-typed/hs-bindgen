# Code structure

- `HsBindgen.Language` contains data structures that are more general; that is,
  not directly related to binding generation, and that could live in separate
  packages.

## Imports in generated code

Generated code may only import from:

1. **qualified modules** — anything imported `qualified` (e.g.
   `HsBindgen.Runtime.Marshal qualified as Marshal`);
2. **support modules** — `HsBindgen.Runtime.Support.*`, the modules of
   `hs-bindgen-runtime` intended for generated code rather than for humans (the
   generated-code prelude `HsBindgen.Runtime.Support` is imported qualified,
   conventionally as `BG`);
3. **sibling modules of the generated program** — when output is split across
   several modules, one generated module may import another (e.g. `import
   Example`).

Generated code imports no prelude unqualified. Anything a binding needs
unqualified comes through the qualified support prelude; a violation of this
rule is a regression.

The support prelude re-exports only definitions meant for unqualified use.
Definitions from `hs-bindgen-runtime` meant for qualified use are imported
directly, qualified, by the generated code (clause 1). See the
`HsBindgen.Runtime.Support` module haddock.
