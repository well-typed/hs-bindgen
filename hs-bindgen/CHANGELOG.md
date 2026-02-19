# Revision history for hs-bindgen

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Occurrences of the `CFieldType`/`CBitfieldType` type families in
  class instance heads are now replaced by their definition.

### New features

* Support top-level anonymous structs and enums as global variables
  (e.g., `struct { int x; int y; } point;`). The anonymous type is named
  after the global variable. Extern anonymous declarations
  (e.g., `extern struct { .. } config;`) are rejected as unusable.

### Minor changes

* Re-export all global definitions used by `hs-bindgen` generated code from
  `hs-bindgen-runtime`. This may affect required packages when using
  `hs-bindgen` generated code. In particular, the packages `ghc-prim` and
  `primitive` are not required by `hs-bindgen` generated code anymore.

### Bug fixes

* Include `FunPtr` for macro-defined newtypes.

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
