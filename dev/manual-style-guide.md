# `hs-bindgen` Manual Style Guide

## Markdown

### Links

Linking to anchors within a page should be done as follows.

```markdown
## This is a section
[t:this-is-a-section]: #this-is-a-section

Reference [this section][t:this-is-a-section] using the tag.
```

Footnotes and other links should be defined at the bottom of the source file,
as follows, separated from the content using *three* blank lines.

```markdown
<!-- footnotes -->

[^1]: This is an example footnote.

<!-- sources and references -->

[manual:introduction]: ../introduction.md
```

There is one link definition per one, and the lines should be sorted
lexicographically.  Targets should be raw, *not* use `<>` syntax (like
`<../introduction.md>`).  Targets in the same directory should *not* use a
`./` prefix.  Directory targets should *not* use a `/` suffix.

Most links should use names like the following.  Note that names should
generally be lowercase and use `-` to separate words.  The exception is
identifiers, where uppercase and `_` characters may be used.

* `clang` to link to the LLVM/Clang homepage

    ```markdown
    [clang]: https://clang.llvm.org/
    ```

* `clang:docs` or `clang:docs:TOPIC` to link to LLVM/Clang documentation
  (*aside from* `libclang` documentation)

    ```markdown
    [clang:docs]: https://clang.llvm.org/docs/index.html
    [clang:docs:cli]: https://clang.llvm.org/docs/ClangCommandLineReference.html
    [clang:docs:cli-include-path]: https://clang.llvm.org/docs/ClangCommandLineReference.html#include-path-management
    ```

* `creference:TOPIC` to link to `en.cppreference.com` documentation

    ```markdown
    [creference:struct]: https://en.cppreference.com/w/c/language/struct.html
    ```

* `example:EXAMPLE` to link to an example

    ```markdown
    [example:bundled-c]: ../../../examples/bundled-c
    ```

* `gcc:docs:TOPIC` to link to GCC documentation

    ```markdown
    [gcc:docs:attributes-visibility]: https://gcc.gnu.org/onlinedocs/gcc/Common-Attributes.html#index-visibility
    ```

* `ghc:guide:TOPIC` to link to the GHC User's Guide

    ```markdown
    [ghc:guide:phases-programs]: https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#replacing-the-program-for-one-or-more-phases
    ```

* `ghc:wiki:TOPIC` to link to the GHC Wiki

    ```markdown
    [ghc:wiki:cross-compiling]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling
    ```

* `hackage:PACKAGE` to link to a package on Hackage

    ```markdown
    hackage:base]: https://hackage.haskell.org/package/base
    ```

* `hackage:PACKAGE:IDENTIFIER` to link to documentation on Hackage

    ```markdown
    [hackage:base:isAlphaNum]: https://hackage.haskell.org/package/base/docs/Data-Char.html#v:isAlphaNum
    ```

* `haskell2010:TOPIC` to link to the Haskell 2010 Report

    ```markdown
    [haskell2010:ffi]: https://www.haskell.org/onlinereport/haskell2010/haskellch8.html
    ```

* `header:FILENAME` to link to a manual header

    ```markdown
    [header:struct.h]: ../../c/structs.h
    ```

* `issue:NUMBER` to link to an `hs-bindgen` issue

    ```markdown
    [issue:1893]: https://github.com/well-typed/hs-bindgen/issues/1893
    ```

* `libclang` to link to `libclang` documentation

    ```markdown
    [libclang]: https://clang.llvm.org/doxygen/group__CINDEX.html
    ```

* `libclang:IDENTIFIER` to link to something specific in `libclang` documentation

    ```markdown
    [libclang:CXCursor]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013
    ```

* `manual:PAGE` to link to a manual page, using `/` for sub-pages that are not in the TOC

    ```markdown
    [manual:clang-options]: clang-options.md
    [manual:structs/nesting]: structs/nesting.md
    ```

* `manual:PAGE-SECTION` to link to a specific section of a manual page

    ```markdown
    [manual:structs/nesting-example-e]: translation/structs/nesting.md#example-e
    ```

* `pr:NUMBER` to link to an `hs-bindgen` pull request

    ```markdown
    [pr:927]: https://github.com/well-typed/hs-bindgen/pull/927
    ```

* `source:FILE` to link to an `hs-bindgen` source file

    ```markdown
    [source:Test01.hs]: ../../../hs-bindgen/test/th/Test/TH/Test01.hs
    ```

* `unfolder:EPISODE` to link to an Unfolder episode, which may include the
  playlist so that people can easily discover other episodes

    ```markdown
    [unfolder:36]: https://www.youtube.com/watch?v=IMrBTx7aYjs&list=PLD8gywOEY4HaG5VSrKVnHxCptlJv2GAn7&index=37
    ```

* `wikipedia:TOPIC` to link to Wikipedia

    ```markdown
    [wikipedia:flam]: https://en.wikipedia.org/wiki/Flexible_array_member
    ```

* Links to a specific blog post or paper should use the full title

    ```markdown
    [Well-Typed: Improving GHC configuration and cross-compilation][]

    ...

    <!-- sources and references -->

    ...
    [Well-Typed: Improving GHC configuration and cross-compilation]: https://well-typed.com/blog/2023/10/improving-ghc-configuration-and-cross-compilation-with-ghc-toolchain/
    ...
    ```

* Other links should either use the full text like links to a specific blog
  post *or* should use names like the above

    ```markdown
    [blog:zw3rk]: https://log.zw3rk.com/
    [qemu:docs:user-mode-emulation]: https://www.qemu.org/docs/master/user/main.html
    [unicode:nfc]: https://unicode.org/reports/tr15/
    ```
