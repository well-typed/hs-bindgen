# Revision history for `c-expr-dsl`

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Re-export parse-related symbols from `C.Expr.Parse`; demote lower-level
  modules to `other-modules`.
* Re-export typecheck-related symbols from `C.Expr.Typecheck`; demote
  `C.Expr.Typecheck.Expr` to `other-modules`; `C.Expr.Typecheck.Type` is still
  an exposed module.

### New features

* Parse macro types in addition to expressions; defer the type-vs-value
  distinction to the typechecking phase. See [PR #1862][pr-1862].
* Add test suite covering the parser (token-based and real-world libclang
  tests) and the typechecker. See [PR #1862][pr-1862].

[pr-1862]: https://github.com/well-typed/hs-bindgen/pull/1862

### Minor changes

### Bug fixes

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
