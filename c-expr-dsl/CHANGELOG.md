# Revision history for `c-expr-dsl`

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Re-export parse-related symbols from `C.Expr.Parse`; demote lower-level
  modules to `other-modules`.
* Re-export typecheck-related symbols from `C.Expr.Typecheck`; demote
  `C.Expr.Typecheck.Expr` to `other-modules`; `C.Expr.Typecheck.Type` is still
  an exposed module.
* `Expr` and `Term` gain a `ctx :: Ctx` type index (from `debruijn`) for
  the local macro parameter scope. `Macro.macroArgs :: [Name]` is replaced
  by an existential `macroParams :: Vec ctx Name`; `macroExpr` becomes
  `Expr ctx Ps`. `sameMacro` compares macros structurally, ignoring location.
* `TypeTagged !TagKind !Name` is now a separate `Literal` constructor
  instead of a `TypeLit` variant.
* Some macros that were previously erroneously parsed as function-like are now
  parsed as object-like. See [PR #1990][pr-1990].
* Remove the `sameMacro` function. See [PR #1983][pr-1983].

### New features

* Parse macro types in addition to expressions; defer the type-vs-value
  distinction to the typechecking phase. See [PR #1862][pr-1862].
* Add test suite covering the parser (token-based and real-world libclang
  tests) and the typechecker. See [PR #1862][pr-1862].
* Local macro parameters in function-like macros are resolved to de Bruijn
  indices (`LocalParam (Idx ctx)`) at parse time, distinguishing them from
  free variables (`Var`).
* `tcMacro` now rejects type-like macros that expand to an incomplete type
  (`void` or `const void` at the top level) with a new `TcIncompleteTypeMacro`
  error. Pointer-to-incomplete types (e.g. `void *`) are still accepted.
* Support multi-line macro definitions. See [PR #1993][pr-1993].

### Minor changes

None

### Bug fixes

* In accordance with the C reference, parse macros only as function-like when
  there is no whitespace between the macro name and the opening parenthesis of
  the parameter list. See [PR #1990][pr-1990].

[pr-1862]: https://github.com/well-typed/hs-bindgen/pull/1862
[pr-1983]: https://github.com/well-typed/hs-bindgen/pull/1983
[pr-1990]: https://github.com/well-typed/hs-bindgen/pull/1990
[pr-1993]: https://github.com/well-typed/hs-bindgen/pull/1993

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
