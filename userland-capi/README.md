# Userland CAPI

Originally CApiFFI was created to solve a convenience problem with `iconv` foreign imports:
https://gitlab.haskell.org/ghc/ghc/-/issues/2979

An important point made already then, is that when creating trivial wrappers,
we need at least to try to make their names unique enough, so the C names won't clash
with some other wrappers. (FWIW, that can easily become issue with names like `hs_sha256_update`!)

Maybe more as a side-effect `CApiFFI` also enabled "type-checking" of C-imports,
as there is a true C code using them compiled with C-compiler, not just linking (which only checked that symbol exists).

However, `CApiFFI` may be considered partial feature. A C-file for wrappers is created, but as programmers we have very limited tools to affect it.
We *could* make an include file with some trivial import, and that way "splice" some contents into the wrapper C file;
the cost is the need to create an extra source file.

Some time later GHC TH got ability to add arbitrary 
code to be compiled and linked together with a module:
[addForeignSource](https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/Language-Haskell-TH-Syntax.html#v:addForeignSource).

`addForeignSource` allows to do the same things `CApiFFI` does:

With `CApiFFI` we do

```haskell
-- we can use CApiFFI to import printf with some fixed signature
foreign import capi "stdio.h printf" my_printf :: CString -> CInt -> IO ()
```

but we can also do

```haskell
-- or we can create wrapper ourselves, all from Haskell source:
$(do
    addForeignSource LangC $ unlines
        [ "#include <stdio.h>"
        , "void printf_wrapper(const char *fmt, int x) { printf(fmt, x); }"
        ]

    [d|
        foreign import ccall "printf_wrapper" my_printf2 :: CString -> CInt -> IO ()
      |])
```

Obviously the latter is more verbose, as we do everything ourselves.
But we can imagine a small library allowing to make imports (and their wrappers) in more concise way.

A first step could be

```haskell
$(userlandCApi 'localname $ do
    addInclude "<stdio.h>"
    w <- freshCName "printf"
    addC $ "void " ++ w ++ "(const char *fmt, int x) { printf(fmt, x); }"
    addDec $ foreignImport w "my_printf2" [t| CString -> CInt -> IO () |])
```

here we use `'localname` to give generator a module unique identifier (unit id together with module name),
from which it can generate unique(-enough) wrapper names.

We can also imagine further adding some utilties to declare C and Hs types simultaneously.

This approach allows to create more elaborate C wrappers (and why not wrappers on Haskell side).
The cost is that approach is not as integrated. `CType` annotations are not reifiable, so we'd need to teach the library that `CInt` is `int` etc.
