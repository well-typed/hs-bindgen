
This example is copied from [a Reddit comment by
Edsko][reddit:ffi-types-example].

This will get a bit technical; I'll try my best to be clear :) (This answer
should probably be in the `hs-bindgen` manual somewhere).

I agree with you; given

```c
int f(int, int);
```

we can, and do, translate this to

```hs
f :: CInt -> CInt -> IO CInt
```

and this is, so far, indeed portable in the sense that "it can be used across
multiple architectures[^2] (with e.g., different bit widths, different byte order,
etc.)".

Unfortunately, the *implementation* of `f` that `hs-bindgen` generates is *not*
portable:


```hs
foreign import ccall safe "f_wrapper" f_wrapper ::
     Int32 -> Int32 -> IO Int32

f :: CInt -> CInt -> IO CInt
f = fromFFIType f_wrapper
```

Note the specific reference to `Int32` here; you might quite reasonably ask why
would we do such a thing. The reason is compositionality of the generated
bindings combined with and an unfortunate quirk of how `foreign import`s and
`Coercible` work in `ghc`.

Suppose we have

```c
// some_other_lib.h
typedef int Foo;

// our_lib.h
#include <some_other_lib.h>
int g(Foo x);
```

and we have an external binding specification that maps `Foo` to some type
`CFoo` in some Haskell library somewhere. What `foreign import` would we
generate for `f`? The most obvious candidate is

```hs
module OurLib where

import SomeOtherLib qualified

foreign import ccall safe "g" g :: SomeOtherLib.CFoo -> IO Int32
```

The problem is that this may not compile. A foreign import like this is only
valid Haskell if `ghc` can determine that `CFoo` is `Coercible` to a type in a
small set of "FFI types". Furthermore, `Coercible` is a weird type class; `ghc`
does not generate any instances of it, but rather resolves `Coercible`
constraints when needed. In order to be able to check whether `CFoo` is
`Coercible` to an FFI type, the constructor for `CFoo`, *and the constructors
for anything that `CFoo` might depend on itself*, must all be in scope. So it
depends on how `CFoo` is defined; if `CFoo` is defined as

```hs
newtype CFoo = CFoo CInt
```

we'd be fine, but if `CFoo` is defined as

```hs
newtype CFoo = CFoo CBar
```

where `CBar` is defined in some other module, the foreign import no longer
compiles, unless we somehow also import the module that defines `CBar`, even
though that is just an implementation detail of `CFoo`. For a while we could
resolve this by insisting that if you have a type intended to use in FFI like
this, and you rely on some other type, you must also re-export the constructors
of that other from your module (transitively). Unfortunately, that does not work
if there are name clashes, for example:

```hs
newtype CFoo = CFoo SomInternalModule.CFoo
```

We also thought about whether we could somehow extend binding specs to record
"additional required imports", but that gets messy also; now a binding spec for
a module in some Haskell package might refer to *other* packages, users would
have to declare more packages in their cabal build-depends field, and in TH mode
we cannot even generate additional imports so users would have to do that by
hand. A huge mess.

So instead we do something different. We have a class `HasFFIType`, which maps any
type to its FFI type, along with conversions

```hs
class HasFFIType a where
  type ToFFIType a :: FFI.FFIType

  toFFIType   :: a -> FFIType a
  fromFFIType :: FFIType a -> a
```

Now we don't care about how `CFoo` is *implemented*, we just care that it has an
`HasFFIType` instance (arguably, something like this is how things should have
been done in ghc in the first place). That doesn't help us in the `foreign
import` itself, of course, so there we instead just use the underlying C type

```c
foreign import ccall safe "g" g_wrapper :: Int32 -> IO Int32

g :: SomeOtherLib.CFoo -> IO Int32
g = fromFFIType g_wrapper
```

That finally still leaves the question about why we translate `CInt` to `Int32`
also. The answer is essentially that `CInt` is another example of a `newtype`
around an FFI type, much like `CFoo` in the example above and so we decided to
treat it in the same way. This felt justifiable partly also because something
like

```c
int f(int, int);
```

may not be quite as portable as it seems if this is actually

```c
#if ..
int f(int, int);
#else ..
..
#endif
```

and `hs-bindgen` cannot detect the difference between these two (or at least not
trivially; `libclang` resolves these CPP conditionals before we get to traverse
the source code).

All that said, *you* as a user might know that these conditional do not exist,
and you might prefer a translation here that is portable. For translating `int`
to `CInt` in foreign imports, or indeed any *primitive* C type, we can do that,
because we can just make sure that Foreign.C is exported; this works because
this is a *known* type with a *known* import. I've opened
https://github.com/well-typed/hs-bindgen/issues/1747 to track this.

Just as a side note: I think the `HasFFIType` class is quite elegant, and also
quite useful; in particular, it also makes it possible to use Haskell types that
are *not* `Coercible` to FFI types, provided you can provide the necessary
translations (though this will require a minor generalization first:
https://github.com/well-typed/hs-bindgen/issues/1565).

