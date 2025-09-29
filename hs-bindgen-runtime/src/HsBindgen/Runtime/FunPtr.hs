module HsBindgen.Runtime.FunPtr (
    Pure (..)
  , castFunPtrSafe
  ) where

import Foreign

-- $setup
--
-- >>> import Foreign.Ptr
-- >>> import Foreign.C.Types

type role Pure nominal
newtype Pure a = Pure { runPure :: a }

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
  -- to a 'FunPtr' of a /pure/ type @... -> Pure a@. Such function pointers are
  -- generated for C functions that have the @const@ function attribute. Such C
  -- functions are considered to be pure in Haskell terminology.
  --
  -- > extern int bar(cchar x) __attribute__((const));
  --
  -- > foreign import ... bar_ptr :: FunPtr (CChar -> Pure CInt)
  --
  -- It is generally safe to use 'castFunPtr' in this case to replace 'Pure' by
  -- 'IO' in the return type. However, 'castFunPtr' has /no constraints at all/,
  -- and it is easy to cast a 'FunPtr' to an incompatible type, likely leading
  -- to segfaults. 'castFunPtrSafe' allows casting in such a way that only
  -- 'Pure' is replaced by 'IO' in the result type of a 'FunPtr'.
  --
  -- >>> let f = castFunPtrSafe @(CChar -> Pure CInt) @(CChar -> IO CInt) -- succeeds
  --
  -- > let g = castFunPtrSafe @(CChar -> IO CInt) @(CChar -> IO CInt) -- fails (not idempotent)
  -- > let h = castFunPtrSafe @(CChar -> Pure CInt) @(IO CInt) -- fails (removed arguments)
  --
  -- Note that 'castFunPtrSafe' is a take on a variadic function. Casting works for
  -- a 'FunPtr' of any number of function arguments.
  --
  -- Finally, we can use @foo@ and @bar@ together with 'castFunPtrSafe':
  --
  -- > castFunPtrSafe bar :: FunPtr (CChar -> IO CInt) -- succeeds
  castFunPtrSafe :: FunPtr a -> FunPtr b

instance CastFunPtrSafe (Pure a) (IO a) where
  castFunPtrSafe = castFunPtr

instance CastFunPtrSafe b b' => CastFunPtrSafe (a -> b) (a -> b') where
  castFunPtrSafe = castFunPtr
