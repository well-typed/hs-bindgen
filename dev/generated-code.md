# Generated code

## Imports

Generated modules enable `NoImplicitPrelude`, so the only names in scope are
the ones they import. They import every module qualified, with two exceptions:

1. **Curated `Prelude` names**, such as `Eq`, `Show`, `IO`, `pure` or `(~)`,
   are imported unqualified with an explicit import list of the names the
   module uses (e.g. `import Prelude (Eq, Show)`). A name belongs to the set
   only if the `Prelude` of every supported `base` version exports it. The set
   is defined by the globals imported from the `Prelude` in
   `HsBindgen.Backend.Global`. The name mangler reserves every name in the
   set in its namespace, so no generated name clashes with it.
2. **Sibling modules**: when output is split across several modules, one
   generated module may import another unqualified (e.g. `import Example`).

All other imports are qualified:

- **Modules meant for qualified import** are imported directly (e.g.
  `HsBindgen.Runtime.Marshal qualified as Marshal`, or the modules named by
  external binding specifications).
- **Modules meant for unqualified import**, such as `Foreign` or `Data.Word`,
  are not imported. Their definitions are re-exported by the support prelude
  `HsBindgen.Runtime.Support`, imported as `BG`, which also bridges
  differences between GHC and `base` versions.

The other `HsBindgen.Runtime.Support.*` modules hold definitions meant for
generated code itself rather than for users of the generated code (e.g.
`BG.CompatHasField`); they are imported qualified. See the
`HsBindgen.Runtime.Support` module haddock.

Template Haskell mode refers to every name by its exact name; these rules
concern the source that `hs-bindgen` writes out.
