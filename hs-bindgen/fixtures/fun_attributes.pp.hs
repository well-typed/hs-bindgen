{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

$(CAPI.addCSource "#include <fun_attributes.h>\nvoid test_internal___f1 (void) { __f1(); }\nvoid test_internal_f1 (void) { f1(); }\nvoid *test_internal_my_memalign (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }\nvoid *test_internal_my_calloc (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }\nvoid *test_internal_my_realloc (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }\nvoid *test_internal_my_alloc1 (size_t arg1) { return my_alloc1(arg1); }\nvoid *test_internal_my_alloc2 (size_t arg1) { return my_alloc2(arg1); }\nsigned int test_internal_square (signed int arg1) { return square(arg1); }\nsigned int test_internal_old_fn (void) { return old_fn(); }\nchar *test_internal_my_dgettext (char *arg1, char *arg2) { return my_dgettext(arg1, arg2); }\nFILE *test_internal_fdopen (signed int arg1, char *arg2) { return fdopen(arg1, arg2); }\nvoid test_internal_f2 (void) { f2(); }\nvoid *test_internal_my_memcpy (void *arg1, void *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }\nvoid test_internal_fatal (void) { fatal(); }\nsigned int test_internal_hash (char *arg1) { return hash(arg1); }\nvoid *test_internal_mymalloc (size_t arg1) { return mymalloc(arg1); }\nvoid test_internal_foobar (void) { foobar(); }\nsigned int test_internal_core2_func (void) { return core2_func(); }\nsigned int test_internal_sse3_func (void) { return sse3_func(); }\nvoid test_internal_f3 (void) { f3(); }\n__attribute__ ((const)) signed int *get_i_ptr (void) { return &i; } \nsigned int test_internal_fn (void) { return fn(); }\nsigned int test_internal_y (void) { return y(); }\nsigned int test_internal_x1 (void) { return x1(); }\nsigned int test_internal_x2 (void) { return x2(); }\n")

{-| Attributes on functions

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

  __from C:__ @FILE@
-}
data FILE = FILE
  {}
  deriving stock (Eq, Show)

instance F.Storable FILE where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure FILE

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE -> return ()

newtype Size_t = Size_t
  { un_Size_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "test_internal___f1" __f1 :: IO ()

foreign import ccall safe "test_internal_f1" f1 :: IO ()

foreign import ccall safe "test_internal_my_memalign" my_memalign :: Size_t -> Size_t -> IO (F.Ptr Void)

foreign import ccall safe "test_internal_my_calloc" my_calloc :: Size_t -> Size_t -> IO (F.Ptr Void)

foreign import ccall safe "test_internal_my_realloc" my_realloc :: (F.Ptr Void) -> Size_t -> IO (F.Ptr Void)

foreign import ccall safe "test_internal_my_alloc1" my_alloc1 :: Size_t -> IO (F.Ptr Void)

foreign import ccall safe "test_internal_my_alloc2" my_alloc2 :: Size_t -> IO (F.Ptr Void)

foreign import ccall safe "test_internal_square" square :: FC.CInt -> FC.CInt

foreign import ccall safe "test_internal_old_fn" old_fn :: IO FC.CInt

foreign import ccall safe "test_internal_my_dgettext" my_dgettext :: (F.Ptr FC.CChar) -> (F.Ptr FC.CChar) -> IO (F.Ptr FC.CChar)

foreign import ccall safe "test_internal_fdopen" fdopen :: FC.CInt -> (F.Ptr FC.CChar) -> IO (F.Ptr FILE)

foreign import ccall safe "test_internal_f2" f2 :: IO ()

foreign import ccall safe "test_internal_my_memcpy" my_memcpy :: (F.Ptr Void) -> (F.Ptr Void) -> Size_t -> IO (F.Ptr Void)

foreign import ccall safe "test_internal_fatal" fatal :: IO ()

{-|

  Marked @__attribute((pure))__@

-}
foreign import ccall safe "test_internal_hash" hash :: (F.Ptr FC.CChar) -> IO FC.CInt

foreign import ccall safe "test_internal_mymalloc" mymalloc :: Size_t -> IO (F.Ptr Void)

foreign import ccall safe "test_internal_foobar" foobar :: IO ()

foreign import ccall safe "test_internal_core2_func" core2_func :: IO FC.CInt

foreign import ccall safe "test_internal_sse3_func" sse3_func :: IO FC.CInt

foreign import ccall safe "test_internal_f3" f3 :: IO ()

foreign import ccall safe "get_i_ptr" i_ptr :: F.Ptr FC.CInt

foreign import ccall safe "test_internal_fn" fn :: IO FC.CInt

foreign import ccall safe "test_internal_y" y :: IO FC.CInt

foreign import ccall safe "test_internal_x1" x1 :: IO FC.CInt

foreign import ccall safe "test_internal_x2" x2 :: IO FC.CInt
