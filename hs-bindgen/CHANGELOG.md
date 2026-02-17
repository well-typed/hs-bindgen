# Revision history for hs-bindgen

## 0.1.0 -- YYYY-mm-dd

* BREAKING: occurrences of the `CFieldType`/`CBitfieldType` type families in
  class instance heads are now replaced by their definition.
* BREAKING: for C function *declarations* that take arrays as arguments,
  generate Haskell function declarations that take pointers to their
  corresponding array elements as arguments. This uses the `Elem` associated
  type class from the new `IsArray` class.
* BREAKING: for C function *types* (.e.g, `typedef`s) that take arrays as
  arguments, generate Haskell function types (.e.g, `newtype`s) that take
  pointers to their corresponding array elements as arguments. This uses the
  `Elem` associated type class from the new `IsArray` class.
* NON-BREAKING: generate an `IsArray` instance for each newtype of a type with
  an `IsArray` instance.
