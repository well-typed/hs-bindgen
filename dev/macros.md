# Macros

This document explains some internal details about how `hs-bindgen` handles C
macros. For the user-facing behaviour, see the [manual](../manual).

## Reparsing: the source range of a declaration

A declaration that uses macros is reparsed from its tokens as written in the
header (macros unexpanded), so that macro names can become Haskell types. The
parse pass collects the original tokens in the header file in `getReparseInfo`
(`HsBindgen.Frontend.Pass.Parse.Decl.Macro`). Finding the start and end
positions of a specific declaration is hard. For example,

```c
1  #define T int
2  #define ARR3 [3]
3  #define PARAMS(args) args
4
5  T x;
6  int y ARR3;
7  T f PARAMS((T a));
   ^^^^^^^^^^^^^^^^^^
   123456789012345678
```

After preprocessing, Clang sees `int x;`, `int y [3];` and `int f (int a);`; we
want `T x`, `int y ARR3` and `T f PARAMS((T a))`.

Clang reports the extent of a declaration as the position of its first token
and the position just after its last token. Clang parses the expanded tokens,
so the first and last token of an extent may be copies made by expanding a
macro; for example, a token of the macro body, or of one of the macro arguments
for function-like macros. These copies exist only in the expanded tokens, not
in the file. We only use their locations to find the declaration in the file.
For macro tokens, `libclang` knows three locations; the table shows them for
two tokens of line 7:

| Location | `int`, the first token of `f` (from `T`) | `)`, the last token of `f` after expansion |
|---|---|---|
| **Spelling**: where the characters are written | 1:11, in `#define T int` | 7:16, the `)` of `(T a)` |
| **File**: where it shows up in the file | 7:1, the invocation `T` | 7:16, inside the argument text |
| **Expansion**: start of the outermost invocation that produced it | 7:1, the invocation `T` | 7:5, the start of `PARAMS` |

In particular, the last token of `f` after expansion is the `)` at 7:16, not
the one at 7:17: that one closes the invocation `PARAMS(...)`, which expansion
removes.

For a token copied from a macro _body_, the _spelling location_ is in the macro
definition (`#define ...`), while the _file location_ is the start of the
invocation. Unless that invocation is itself written in a macro argument, as
`T` in `PARAMS((T a))`, this is the start of the outermost invocation, the same
point as its expansion location.

For a token copied from a macro _argument_, the spelling location and the file
location coincide: both are where the token is written in the argument, inside
the invocation.

The _expansion location_ of either token is the start of the outermost
invocation: a single point; `libclang` has no API for the end of an invocation.

The range we want for `f` runs from 7:1 to 7:18, just after the final `)`. The
expansion location of the start is right, 7:1; that of the end is wrong, 7:5.
In general, an end of an extent lies in one of three places:

| End lies… | Example | Expansion location of the end | Correct? |
|---|---|---|---|
| in plain file text | line 5, after `x` | 5:4, the position itself | yes |
| in a macro body | line 6, after `]` from `ARR3` | 6:11, after the invocation `ARR3` | yes |
| in a macro argument | line 7, after `)` from `(T a)` | 7:5, the _start_ of `PARAMS` | no |

The middle row works because `libclang` moves an extent end in a macro body to
the end of the invocation before handing it out. An extent end in a macro
argument stays inside the argument, just after the `)` at 7:16, and its
expansion location is the start of the invocation. A start needs no such help:
wherever it lies, the start of the outermost invocation is where the written
declaration begins.

So the last row is the only edge case. We take the end of the invocation from
the macro invocations recorded while parsing: each carries the extent of the
whole invocation, here 7:5 to 7:18. Two rules apply (`getReparseInfo`):

- Extend the end only if it lies in a macro argument, that is, if its file
  location differs from its expansion location. An invocation that merely
  starts where a declaration ends, as `ID(;)` in `T arr[3]ID(;)`, is not part of
  the declaration; the end of the range is exclusive, also when we look up the
  invocations in it.
- Look up the invocations used by the declaration in the _extended_ range.
  Otherwise the invocations nested in the argument, such as `U` in
  `int g PARAMS((U a))`, are missed, and the reparser does not know that `U`
  names a type.
