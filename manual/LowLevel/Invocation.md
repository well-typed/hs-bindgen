# Running `hs-bindgen`

## CLI

Preprocessor mode

## Template Haskell

## Selection

For which C names do we want to generate Haskell definitions?

## `hs-bindgen`-as-a-library

## Cross-compilation

## Environment variables

A small set of environment variables affects the behavior of `hs-bindgen`.

Users can provide extra command-line arguments to `libclang`:

- __When compiling natively (i.e., without specifying a target)__: `hs-bindgen`
  reads
  `BINDGEN_EXTRA_CLANG_ARGS` and splits its string value into command-line
  arguments, respecting shell escapes. For example,

  ```sh
  BINDGEN_EXTRA_CLANG_ARGS="arg1\ with\ whitespace\ endOfArg1 arg2"
  BINDGEN_EXTRA_CLANG_ARGS="\"arg1 with whitespace endOfArg1\" arg2"
  ```

- __When cross-compiling to a given target__: `hs-bindgen` uses
  `BINDGEN_EXTRA_CLANG_ARGS_<TARGET>` instead of `BINDGEN_EXTRA_CLANG_ARGS`.
  Supported `<TARGET>`s: `x86_64-pc-linux`, `i386-pc-linux`, `aarch64-pc-linux`,
  `x86_64-pc-windows`, `x86_64-apple-macosx`, `aarch64-apple-macosx`.
  `hs-bindgen` falls back to `BINDGEN_EXTRA_CLANG_ARGS` if the target-specific
  environment variable is unset or empty. Therefore, a non-empty,
  target-specific variable takes precedence, and `hs-bindgen` ignores
  `BINDGEN_EXTRA_CLANG_ARGS`.

This behavior is consistent with that of
[`rust-bindgen`](https://github.com/rust-lang/rust-bindgen?tab=readme-ov-file#environment-variables).

Configuration of `libclang` via environment variables is important because:

- Software distribution maintainers (e.g., for NixOS) may want to configure
  `hs-bindgen`'s behavior for packages that use it.

- Users may want to configure `hs-bindgen`'s behavior when it is invoked via
  Template Haskell (using `hashInclude` or `hashIncludeWith`), without modifying
  the source code.

Note that when using `hs-bindgen-cli`, users can fully configure `libclang`
with command-line arguments. For instance, they can set:

- `--system-include-path DIR`,
- `--include-path DIR`, and
- `--clang-option OPTION` (for other options passed directly to `libclang`).
