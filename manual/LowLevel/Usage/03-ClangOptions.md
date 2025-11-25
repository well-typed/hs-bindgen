# Clang options

`hs-bindgen` uses the [`libclang`][] interface to the [Clang][] compiler to
parse C header files.  Clang provides various options that determine how the
header files are parsed.  Note that, in some cases, options affect which
preprocessor macros are defined.

[`libclang`]: <https://clang.llvm.org/doxygen/group__CINDEX.html>
[Clang]: <https://clang.llvm.org/>

## Command-line options

`hs-bindgen` passes [Clang command-line options][] to `libclang`.  Some common
options are managed by `hs-bindgen`, but you can configure arbitrary options as
well.

[Clang command-line options]: <https://clang.llvm.org/docs/ClangCommandLineReference.html>

When using `hs-bindgen-cli preprocess`, Clang options may be specified as
command-line options.  Common options are exposed as `hs-bindgen-cli`
command-line options, while arbitrary Clang options may be passed using
`--clang-option-before`, `--clang-option`, and `--clang-option-after`.

Options are passed to Clang in the following order:

1. `--clang-option-before` options
2. Clang options managed by `hs-bindgen` (target, C standard, include
  directories, etc.)
3. `--clang-option` options
4. `BINDGEN_EXTRA_CLANG_ARGS` options (see below)
5. `--clang-option-after` options

See `hs-bindgen-cli preprocess --help` for details about which options are
managed by `hs-bindgen`.

Example:

```
$ hs-bindgen-cli preprocess \
    --standard c23 \
    -I include \
    --clang-option="-idirafter/opt/acme-0.1.0/include" \
    --module Foo \
    --output Foo.hs \
    foo.h
```

## Environment variables

Clang options may also be set using environment variables.  This is
particularly useful when setting environment-specific configuration that may
not be hard-coded in the source code.  Options specified via the CLI or the
Template Haskell API take precedence.

- __When compiling natively (i.e., without specifying a target)__, `hs-bindgen`
  reads `BINDGEN_EXTRA_CLANG_ARGS` and splits its string value into command-line
  arguments, respecting shell escapes.  Example:

    ```sh
    BINDGEN_EXTRA_CLANG_ARGS="arg1\ with\ whitespace\ endOfArg1 arg2"
    BINDGEN_EXTRA_CLANG_ARGS="\"arg1 with whitespace endOfArg1\" arg2"
    ```

- __When cross-compiling to a given target__, `hs-bindgen` uses
  `BINDGEN_EXTRA_CLANG_ARGS_<TARGET>` instead of `BINDGEN_EXTRA_CLANG_ARGS`.
  A non-empty target-specific configuration takes precedence, and `hs-bindgen`
  ignores `BINDGEN_EXTRA_CLANG_ARGS`.  `hs-bindgen` falls back to
  `BINDGEN_EXTRA_CLANG_ARGS` if the target-specific environment variable is
  unset or empty.

  The following targets are supported:

    - `x86_64-pc-linux`
    - `i386-pc-linux`
    - `aarch64-pc-linux`
    - `x86_64-pc-windows`
    - `x86_64-apple-macosx`
    - `aarch64-apple-macosx`

This behavior is consistent with that of [`rust-bindgen`][].

[`rust-bindgen`]: <https://github.com/rust-lang/rust-bindgen?tab=readme-ov-file#environment-variables>
