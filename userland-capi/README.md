# Userland CAPI

Originally CApiFFI was created to solve a convenience problem with `iconv`
foreign imports: https://gitlab.haskell.org/ghc/ghc/-/issues/2979

An important point made already then, is that when creating trivial wrappers, we
need at least to try to make their names unique enough, so the C names won't
clash with some other wrappers. (FWIW, that can easily become an issue with
names like `hs_sha256_update`!)

Maybe more as a side-effect, the `CApiFFI` calling convention also enabled
"type-checking" of C imports: `CApiFFI` generates actual C code using the
C imports; this C code is then compiled with a C compiler, and not merely linked
(which only checks that symbols exists).

However, `CApiFFI` may be considered a partial feature. A C file containing the
wrappers is created, but as programmers we have limited control over this C
file. We *could* make an include file with some trivial import, and that way
"splice" some contents into the wrapper C file; the cost is the need to create
an extra source file.

Some time later GHC Template Haskell (TH) got ability to add arbitrary code to
be compiled and linked together with a module:
[addForeignSource](https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/Language-Haskell-TH-Syntax.html#v:addForeignSource).

`addForeignSource` allows to do the same things `CApiFFI` does:

With `CApiFFI` we can import `printf` with a fixed signature
```haskell
foreign import capi "stdio.h printf" my_printf :: CString -> CInt -> IO ()
```

We can also create wrappers ourselves, directly from Haskell
```haskell
$(do
    addForeignSource LangC $ unlines
        [ "#include <stdio.h>"
        , "void printf_wrapper(const char *fmt, int x) { printf(fmt, x); }"
        ]

    [d|
        foreign import ccall "printf_wrapper" my_printf2 :: CString -> CInt -> IO ()
      |])
```

Obviously the latter is more verbose, as we do everything ourselves. But we can
imagine a small library allowing to make imports (and their wrappers) in a more
concise way.

A first step could be

```haskell
$(userlandCApi 'localname $ do
    addInclude "<stdio.h>"
    w <- freshCName "printf"
    addC $ "void " ++ w ++ "(const char *fmt, int x) { printf(fmt, x); }"
    addDec $ foreignImport w "my_printf2" [t| CString -> CInt -> IO () |])
```

Here, `'localname` is a module-unique identifier (i.e., a unit ID together with
the module name), from which the userland CAPI generates unique(-enough) wrapper
names.

We can also imagine further adding some utilties to declare C and Haskell types
simultaneously.

This approach allows to create more elaborate C wrappers (and, why not, wrappers
on the Haskell side). The cost is that the approach is not integrated as well as
the CAPI calling convention provided by GHC. For example, `CType` annotations
are not reifiable, so we wouldd need to teach the library that `CInt` is `int`.
