module HsBindgen.Runtime.FunPtr (
    castFunPtrSafe
  ) where

import Foreign

class CastFunPtrSafe a b where
  -- | Cast 'FunPtr's safely.
  --
  -- When passing functions to C code through the FFI, there are cases where one
  -- wants to pass a 'FunPtr' of a slightly different but compatible type to the
  -- one that is required by a foreign import, but Haskell rejects the 'FunPtr'
  -- because it is not exactly of the required type. In such cases
  -- 'castFunPtrSafe' allows casting a 'FunPtr' to the right type /safely/.
  --
  -- For example, we could have a binding to a C function @foo@ that takes a
  -- function pointer as an argument.
  --
  -- > extern bool foo(int (*f)(cchar));
  --
  -- > foreign import ... foo :: FunPtr (CChar -> IO CInt) -> IO CBool
  --
  -- Generally, if a C function takes a function pointer argument, then the
  -- accompanying Haskell binding takes a 'FunPtr' of a type that roughly looks
  -- like @... -> 'IO' a@. However, in Haskell land one might have access only
  -- to a 'FunPtr' of a /pure/ type @... -> a@, like @someFunPtr@.
  --
  -- > someFunPtr :: FunPtr (CChar -> CInt)
  --
  -- It is generally safe to use 'castFunPtr' in this case to introduce 'IO' in
  -- the return type. However, 'castFunPtr' has /no constraints at all/, and it
  -- is easy to cast a 'FunPtr' to an incompatible type, leading to segfaults.
  -- 'castFunPtrSafe' allows casting in such a way that only 'IO' is introduced
  -- in the result type of a 'FunPtr'.
  --
  -- > castFunPtrSafe @(CChar -> CInt)    @(CChar -> IO CInt) -- succeeds
  -- > castFunPtrSafe @(CChar -> IO CInt) @(CChar -> IO CInt) -- fails (not idempotent)
  -- > castFunPtrSafe @(CChar -> CInt)    @(IO CInt) -- fails (removed arguments)
  --
  -- Note that 'castFunPtrSafe' is a take on a variadic function. Casting works for
  -- a 'FunPtr' of any number of function arguments.
  --
  -- Finally, we can use @foo@ and @someFunPtr@ together with 'castFunPtrSafe':
  --
  -- > foo (castFunPtrSafe someFunPtr) -- succeeds
  castFunPtrSafe :: FunPtr a -> FunPtr b

instance CastFunPtrSafe a (IO a) where
  castFunPtrSafe = castFunPtr

instance CastFunPtrSafe b b' => CastFunPtrSafe (a -> b) (a -> b') where
  castFunPtrSafe = castFunPtr
