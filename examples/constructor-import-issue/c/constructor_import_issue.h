// We generate bindings for A in module A
typedef char A;

// We generate bindings for B in module B, using the binding spec for A as input
typedef A B;

// We generate bindings for the remaining declarations using thge binding spec
// for A and the binding spec for B as inputs.
extern B ex_global;
extern B ex_func (B x);
typedef B ex_func_tydef (B x);
typedef B (*ex_func_ptr_tydef) (B x);

// Importantly, the generated Haskell bindings for the ex_* declarations import
// module B, but not module A. As a result, the constructor for B is in scope,
// but the constructor for A is not. This can lead to Haskell compiler errors,
// unless we use base foreign types. See issue
// [#1282](https://github.com/well-typed/hs-bindgen/issues/1282).