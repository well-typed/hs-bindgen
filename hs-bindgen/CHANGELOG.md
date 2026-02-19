# Revision history for hs-bindgen

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Occurrences of the `CFieldType`/`CBitfieldType` type families in
  class instance heads are now replaced by their definition.

### New features

* Add `--post-qualified-imports` flag to generate post-qualified imports
  (`import Data.Proxy qualified`) instead of pre-qualified imports. This adds
  the `ImportQualifiedPost` language extension to generated modules.

### Minor changes

* Re-export all global definitions used by `hs-bindgen` generated code from
  `hs-bindgen-runtime`. This may affect required packages when using
  `hs-bindgen` generated code. In particular, the packages `ghc-prim` and
  `primitive` are not required by `hs-bindgen` generated code anymore.

### Bug fixes

* Include `FunPtr` for macro-defined newtypes.

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
