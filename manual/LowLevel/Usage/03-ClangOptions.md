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
`--clang-option-before`, `--clang-option`, `--clang-option-after`, or
the environment variable `BINDGEN_EXTRA_CLANG_ARGS`.

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

```console
hs-bindgen-cli preprocess \
  --standard c23 \
  -I include \
  --clang-option="-idirafter/opt/acme-0.1.0/include" \
  --module Foo \
  --hs-output-dir src \
  foo.h
```

## Environment variables

Clang options may also be set using the `BINDGEN_EXTRA_CLANG_ARGS` environment
variable.  This is particularly useful when setting environment-specific
configuration that may not be hard-coded in the source code.  Options specified
via the CLI or the Template Haskell API take precedence.

Values are split into command-line arguments, respecting shell escapes.  For
example, the following two values parse to the same arguments:

```sh
BINDGEN_EXTRA_CLANG_ARGS="arg1\ with\ whitespace\ endOfArg1 arg2"
BINDGEN_EXTRA_CLANG_ARGS="\"arg1 with whitespace endOfArg1\" arg2"
```

Note that use of this environment variable is consistent with [`rust-bindgen`],
but we do *not* support target-specific environment variables.

[`rust-bindgen`]: <https://github.com/rust-lang/rust-bindgen?tab=readme-ov-file#environment-variables>
