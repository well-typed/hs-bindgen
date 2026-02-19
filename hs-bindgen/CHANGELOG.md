# Revision history for hs-bindgen

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Occurrences of the `CFieldType`/`CBitfieldType` type families in
  class instance heads are now replaced by their definition.

### New features

### Minor changes

* Re-export all global definitions used by `hs-bindgen` generated code from
  `hs-bindgen-runtime`. This may affect required packages when using
  `hs-bindgen` generated code. In particular, the packages `ghc-prim` and
  `primitive` are not required by `hs-bindgen` generated code anymore.

### Bug fixes

* Fix incorrect enum constant values for enums with unsigned underlying types
  (e.g. `enum : uint8_t`). Values above the signed range (such as 128 or 255
  for `uint8_t`) were incorrectly stored as negative numbers because
  `hs-bindgen` used the signed libclang API.

* Include `FunPtr` for macro-defined newtypes.

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
