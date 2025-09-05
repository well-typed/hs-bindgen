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
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

$(CAPI.addCSource "#include <fun_attributes.h>\nvoid hs_bindgen_test_fun_attributes_d2d46ab14aa4b1f9 (void) { __f1(); }\n/* get___f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_2e369f5f60ff28c5 (void)) (void) { return &__f1; } \nvoid hs_bindgen_test_fun_attributes_8b60d38de80093fa (void) { f1(); }\n/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_a1b79fe9af8e18b8 (void)) (void) { return &f1; } \nvoid *hs_bindgen_test_fun_attributes_72e7e9398b70632a (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }\n/* get_my_memalign_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_df18e1ec526fabcf (void)) (size_t arg1, size_t arg2) { return &my_memalign; } \nvoid *hs_bindgen_test_fun_attributes_1e1fd866f4d88373 (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }\n/* get_my_calloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_1371a36b12f9bdfc (void)) (size_t arg1, size_t arg2) { return &my_calloc; } \nvoid *hs_bindgen_test_fun_attributes_3c7e2f0546d7f0f8 (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }\n/* get_my_realloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_149f0ffc9a274b08 (void)) (void *arg1, size_t arg2) { return &my_realloc; } \nvoid *hs_bindgen_test_fun_attributes_4cb3f4400795f3dc (size_t arg1) { return my_alloc1(arg1); }\n/* get_my_alloc1_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_8764901d3de7c252 (void)) (size_t arg1) { return &my_alloc1; } \nvoid *hs_bindgen_test_fun_attributes_e3dd92fe5b87fb45 (size_t arg1) { return my_alloc2(arg1); }\n/* get_my_alloc2_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_0e2a7c466f494b53 (void)) (size_t arg1) { return &my_alloc2; } \nsigned int hs_bindgen_test_fun_attributes_8effe939268709e4 (signed int arg1) { return square(arg1); }\n/* get_square_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_631c7b52d4d4fe3a (void)) (signed int arg1) { return &square; } \nsigned int hs_bindgen_test_fun_attributes_1dddc7f5a16104d4 (void) { return old_fn(); }\n/* get_old_fn_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_407cc567cd7ef4a1 (void)) (void) { return &old_fn; } \nchar *hs_bindgen_test_fun_attributes_77f81f76a170977e (char *arg1, char const *arg2) { return my_dgettext(arg1, arg2); }\n/* get_my_dgettext_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_fun_attributes_60702a9764046d9e (void)) (char *arg1, char const *arg2) { return &my_dgettext; } \nFILE *hs_bindgen_test_fun_attributes_d97c2ae9c1dff04d (signed int arg1, char const *arg2) { return fdopen(arg1, arg2); }\n/* get_fdopen_ptr */ __attribute__ ((const)) FILE *(*hs_bindgen_test_fun_attributes_e8eae9d0dd40ede4 (void)) (signed int arg1, char const *arg2) { return &fdopen; } \nvoid hs_bindgen_test_fun_attributes_4a86b0420a250963 (void) { f2(); }\n/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_74cfd16f2b7e27ba (void)) (void) { return &f2; } \nvoid *hs_bindgen_test_fun_attributes_bcbe640b60445a4f (void *arg1, void const *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }\n/* get_my_memcpy_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_de9d3228e8bac25c (void)) (void *arg1, void const *arg2, size_t arg3) { return &my_memcpy; } \nvoid hs_bindgen_test_fun_attributes_fd569d78d0ba9fd9 (void) { fatal(); }\n/* get_fatal_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_4a9c795c1867222e (void)) (void) { return &fatal; } \nsigned int hs_bindgen_test_fun_attributes_71214e4420f53a0e (char *arg1) { return hash(arg1); }\n/* get_hash_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_241bedb74b8016f3 (void)) (char *arg1) { return &hash; } \nvoid *hs_bindgen_test_fun_attributes_a71e3488215ca2b1 (size_t arg1) { return mymalloc(arg1); }\n/* get_mymalloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_adfbe41965d544a3 (void)) (size_t arg1) { return &mymalloc; } \nvoid hs_bindgen_test_fun_attributes_f2d6c9a4f06efd88 (void) { foobar(); }\n/* get_foobar_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_730ef6ad01273b1d (void)) (void) { return &foobar; } \nsigned int hs_bindgen_test_fun_attributes_ab8f0d32c1f84295 (void) { return core2_func(); }\n/* get_core2_func_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_abb5c394ed250f25 (void)) (void) { return &core2_func; } \nsigned int hs_bindgen_test_fun_attributes_f50d1e8063148c18 (void) { return sse3_func(); }\n/* get_sse3_func_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_12616d0501d14a7a (void)) (void) { return &sse3_func; } \nvoid hs_bindgen_test_fun_attributes_1b95ce9d55223970 (void) { f3(); }\n/* get_f3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_08809dca6bfda237 (void)) (void) { return &f3; } \n/* get_i_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_fun_attributes_fe81510d355aff25 (void) { return &i; } \nsigned int hs_bindgen_test_fun_attributes_43b222bddec511f3 (void) { return fn(); }\n/* get_fn_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_9d0d1087d0fa4b10 (void)) (void) { return &fn; } \nsigned int hs_bindgen_test_fun_attributes_fd90ce98862f93f3 (void) { return y(); }\n/* get_y_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_2d9291944d890d18 (void)) (void) { return &y; } \nsigned int hs_bindgen_test_fun_attributes_8dadf866461c7be6 (void) { return x1(); }\n/* get_x1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_8839814efdc23f88 (void)) (void) { return &x1; } \nsigned int hs_bindgen_test_fun_attributes_31759f8ffef2c6b0 (void) { return x2(); }\n/* get_x2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_c63d8c58f9a27a01 (void)) (void) { return &x2; } \n")

{-| Attributes on functions

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @FILE@

__defined at:__ @fun_attributes.h:7:9@

__exported by:__ @fun_attributes.h@
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

{-| __C declaration:__ @size_t@

    __defined at:__ @fun_attributes.h:8:13@

    __exported by:__ @fun_attributes.h@
-}
newtype Size_t = Size_t
  { un_Size_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @__f1@

    __defined at:__ @fun_attributes.h:16:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_d2d46ab14aa4b1f9" __f1
  :: IO ()

{-| __C declaration:__ @__f1@

    __defined at:__ @fun_attributes.h:16:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_2e369f5f60ff28c5" hs_bindgen_test_fun_attributes_2e369f5f60ff28c5
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE __f1_ptr #-}

__f1_ptr :: F.FunPtr (IO ())
__f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_2e369f5f60ff28c5

{-| __C declaration:__ @f1@

    __defined at:__ @fun_attributes.h:17:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_8b60d38de80093fa" f1
  :: IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @fun_attributes.h:17:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_a1b79fe9af8e18b8" hs_bindgen_test_fun_attributes_a1b79fe9af8e18b8
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

f1_ptr :: F.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_a1b79fe9af8e18b8

{-| __C declaration:__ @my_memalign@

    __defined at:__ @fun_attributes.h:21:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_72e7e9398b70632a" my_memalign
  :: Size_t
  -> Size_t
  -> IO (F.Ptr Void)

{-| __C declaration:__ @my_memalign@

    __defined at:__ @fun_attributes.h:21:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_df18e1ec526fabcf" hs_bindgen_test_fun_attributes_df18e1ec526fabcf
  :: IO (F.FunPtr (Size_t -> Size_t -> IO (F.Ptr Void)))

{-# NOINLINE my_memalign_ptr #-}

my_memalign_ptr :: F.FunPtr (Size_t -> Size_t -> IO (F.Ptr Void))
my_memalign_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_df18e1ec526fabcf

{-| __C declaration:__ @my_calloc@

    __defined at:__ @fun_attributes.h:26:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_1e1fd866f4d88373" my_calloc
  :: Size_t
  -> Size_t
  -> IO (F.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @fun_attributes.h:26:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_1371a36b12f9bdfc" hs_bindgen_test_fun_attributes_1371a36b12f9bdfc
  :: IO (F.FunPtr (Size_t -> Size_t -> IO (F.Ptr Void)))

{-# NOINLINE my_calloc_ptr #-}

my_calloc_ptr :: F.FunPtr (Size_t -> Size_t -> IO (F.Ptr Void))
my_calloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_1371a36b12f9bdfc

{-| __C declaration:__ @my_realloc@

    __defined at:__ @fun_attributes.h:27:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_3c7e2f0546d7f0f8" my_realloc
  :: F.Ptr Void
  -> Size_t
  -> IO (F.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @fun_attributes.h:27:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_149f0ffc9a274b08" hs_bindgen_test_fun_attributes_149f0ffc9a274b08
  :: IO (F.FunPtr ((F.Ptr Void) -> Size_t -> IO (F.Ptr Void)))

{-# NOINLINE my_realloc_ptr #-}

my_realloc_ptr :: F.FunPtr ((F.Ptr Void) -> Size_t -> IO (F.Ptr Void))
my_realloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_149f0ffc9a274b08

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @fun_attributes.h:32:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_4cb3f4400795f3dc" my_alloc1
  :: Size_t
  -> IO (F.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @fun_attributes.h:32:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_8764901d3de7c252" hs_bindgen_test_fun_attributes_8764901d3de7c252
  :: IO (F.FunPtr (Size_t -> IO (F.Ptr Void)))

{-# NOINLINE my_alloc1_ptr #-}

my_alloc1_ptr :: F.FunPtr (Size_t -> IO (F.Ptr Void))
my_alloc1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_8764901d3de7c252

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @fun_attributes.h:33:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_e3dd92fe5b87fb45" my_alloc2
  :: Size_t
  -> IO (F.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @fun_attributes.h:33:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_0e2a7c466f494b53" hs_bindgen_test_fun_attributes_0e2a7c466f494b53
  :: IO (F.FunPtr (Size_t -> IO (F.Ptr Void)))

{-# NOINLINE my_alloc2_ptr #-}

my_alloc2_ptr :: F.FunPtr (Size_t -> IO (F.Ptr Void))
my_alloc2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_0e2a7c466f494b53

{-| __C declaration:__ @square@

    __defined at:__ @fun_attributes.h:37:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_8effe939268709e4" square
  :: FC.CInt
  -> FC.CInt

{-| __C declaration:__ @square@

    __defined at:__ @fun_attributes.h:37:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_631c7b52d4d4fe3a" hs_bindgen_test_fun_attributes_631c7b52d4d4fe3a
  :: IO (F.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

square_ptr :: F.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_631c7b52d4d4fe3a

{-| __C declaration:__ @old_fn@

    __defined at:__ @fun_attributes.h:46:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_1dddc7f5a16104d4" old_fn
  :: IO FC.CInt

{-| __C declaration:__ @old_fn@

    __defined at:__ @fun_attributes.h:46:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_407cc567cd7ef4a1" hs_bindgen_test_fun_attributes_407cc567cd7ef4a1
  :: IO (F.FunPtr (IO FC.CInt))

{-# NOINLINE old_fn_ptr #-}

old_fn_ptr :: F.FunPtr (IO FC.CInt)
old_fn_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_407cc567cd7ef4a1

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @fun_attributes.h:57:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_77f81f76a170977e" my_dgettext
  :: F.Ptr FC.CChar
     {- ^ __C declaration:__ @my_domain@
     -}
  -> F.Ptr FC.CChar
     {- ^ __C declaration:__ @my_format@
     -}
  -> IO (F.Ptr FC.CChar)

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @fun_attributes.h:57:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_60702a9764046d9e" hs_bindgen_test_fun_attributes_60702a9764046d9e
  :: IO (F.FunPtr ((F.Ptr FC.CChar) -> (F.Ptr FC.CChar) -> IO (F.Ptr FC.CChar)))

{-# NOINLINE my_dgettext_ptr #-}

my_dgettext_ptr :: F.FunPtr ((F.Ptr FC.CChar) -> (F.Ptr FC.CChar) -> IO (F.Ptr FC.CChar))
my_dgettext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_60702a9764046d9e

{-| __C declaration:__ @fdopen@

    __defined at:__ @fun_attributes.h:68:9@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_d97c2ae9c1dff04d" fdopen
  :: FC.CInt
  -> F.Ptr FC.CChar
  -> IO (F.Ptr FILE)

{-| __C declaration:__ @fdopen@

    __defined at:__ @fun_attributes.h:68:9@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_e8eae9d0dd40ede4" hs_bindgen_test_fun_attributes_e8eae9d0dd40ede4
  :: IO (F.FunPtr (FC.CInt -> (F.Ptr FC.CChar) -> IO (F.Ptr FILE)))

{-# NOINLINE fdopen_ptr #-}

fdopen_ptr :: F.FunPtr (FC.CInt -> (F.Ptr FC.CChar) -> IO (F.Ptr FILE))
fdopen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_e8eae9d0dd40ede4

{-| __C declaration:__ @f2@

    __defined at:__ @fun_attributes.h:72:65@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_4a86b0420a250963" f2
  :: IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @fun_attributes.h:72:65@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_74cfd16f2b7e27ba" hs_bindgen_test_fun_attributes_74cfd16f2b7e27ba
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

f2_ptr :: F.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_74cfd16f2b7e27ba

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @fun_attributes.h:78:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_bcbe640b60445a4f" my_memcpy
  :: F.Ptr Void
     {- ^ __C declaration:__ @dest@
     -}
  -> F.Ptr Void
     {- ^ __C declaration:__ @src@
     -}
  -> Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (F.Ptr Void)

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @fun_attributes.h:78:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_de9d3228e8bac25c" hs_bindgen_test_fun_attributes_de9d3228e8bac25c
  :: IO (F.FunPtr ((F.Ptr Void) -> (F.Ptr Void) -> Size_t -> IO (F.Ptr Void)))

{-# NOINLINE my_memcpy_ptr #-}

my_memcpy_ptr :: F.FunPtr ((F.Ptr Void) -> (F.Ptr Void) -> Size_t -> IO (F.Ptr Void))
my_memcpy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_de9d3228e8bac25c

{-| __C declaration:__ @fatal@

    __defined at:__ @fun_attributes.h:95:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_fd569d78d0ba9fd9" fatal
  :: IO ()

{-| __C declaration:__ @fatal@

    __defined at:__ @fun_attributes.h:95:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_4a9c795c1867222e" hs_bindgen_test_fun_attributes_4a9c795c1867222e
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE fatal_ptr #-}

fatal_ptr :: F.FunPtr (IO ())
fatal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_4a9c795c1867222e

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @fun_attributes.h:103:5@

__exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_71214e4420f53a0e" hash
  :: F.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @hash@

    __defined at:__ @fun_attributes.h:103:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_241bedb74b8016f3" hs_bindgen_test_fun_attributes_241bedb74b8016f3
  :: IO (F.FunPtr ((F.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE hash_ptr #-}

hash_ptr :: F.FunPtr ((F.Ptr FC.CChar) -> IO FC.CInt)
hash_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_241bedb74b8016f3

{-| __C declaration:__ @mymalloc@

    __defined at:__ @fun_attributes.h:108:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_a71e3488215ca2b1" mymalloc
  :: Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (F.Ptr Void)

{-| __C declaration:__ @mymalloc@

    __defined at:__ @fun_attributes.h:108:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_adfbe41965d544a3" hs_bindgen_test_fun_attributes_adfbe41965d544a3
  :: IO (F.FunPtr (Size_t -> IO (F.Ptr Void)))

{-# NOINLINE mymalloc_ptr #-}

mymalloc_ptr :: F.FunPtr (Size_t -> IO (F.Ptr Void))
mymalloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_adfbe41965d544a3

{-| __C declaration:__ @foobar@

    __defined at:__ @fun_attributes.h:112:13@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_f2d6c9a4f06efd88" foobar
  :: IO ()

{-| __C declaration:__ @foobar@

    __defined at:__ @fun_attributes.h:112:13@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_730ef6ad01273b1d" hs_bindgen_test_fun_attributes_730ef6ad01273b1d
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE foobar_ptr #-}

foobar_ptr :: F.FunPtr (IO ())
foobar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_730ef6ad01273b1d

{-| __C declaration:__ @core2_func@

    __defined at:__ @fun_attributes.h:119:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_ab8f0d32c1f84295" core2_func
  :: IO FC.CInt

{-| __C declaration:__ @core2_func@

    __defined at:__ @fun_attributes.h:119:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_abb5c394ed250f25" hs_bindgen_test_fun_attributes_abb5c394ed250f25
  :: IO (F.FunPtr (IO FC.CInt))

{-# NOINLINE core2_func_ptr #-}

core2_func_ptr :: F.FunPtr (IO FC.CInt)
core2_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_abb5c394ed250f25

{-| __C declaration:__ @sse3_func@

    __defined at:__ @fun_attributes.h:120:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_f50d1e8063148c18" sse3_func
  :: IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @fun_attributes.h:120:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_12616d0501d14a7a" hs_bindgen_test_fun_attributes_12616d0501d14a7a
  :: IO (F.FunPtr (IO FC.CInt))

{-# NOINLINE sse3_func_ptr #-}

sse3_func_ptr :: F.FunPtr (IO FC.CInt)
sse3_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_12616d0501d14a7a

{-| __C declaration:__ @f3@

    __defined at:__ @fun_attributes.h:124:49@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_1b95ce9d55223970" f3
  :: IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @fun_attributes.h:124:49@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_08809dca6bfda237" hs_bindgen_test_fun_attributes_08809dca6bfda237
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

f3_ptr :: F.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_08809dca6bfda237

{-| __C declaration:__ @i@

    __defined at:__ @fun_attributes.h:125:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_fe81510d355aff25" hs_bindgen_test_fun_attributes_fe81510d355aff25
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i_ptr #-}

i_ptr :: F.Ptr FC.CInt
i_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_fe81510d355aff25

{-| __C declaration:__ @fn@

    __defined at:__ @fun_attributes.h:129:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_43b222bddec511f3" fn
  :: IO FC.CInt

{-| __C declaration:__ @fn@

    __defined at:__ @fun_attributes.h:129:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_9d0d1087d0fa4b10" hs_bindgen_test_fun_attributes_9d0d1087d0fa4b10
  :: IO (F.FunPtr (IO FC.CInt))

{-# NOINLINE fn_ptr #-}

fn_ptr :: F.FunPtr (IO FC.CInt)
fn_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_9d0d1087d0fa4b10

{-| __C declaration:__ @y@

    __defined at:__ @fun_attributes.h:135:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_fd90ce98862f93f3" y
  :: IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @fun_attributes.h:135:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_2d9291944d890d18" hs_bindgen_test_fun_attributes_2d9291944d890d18
  :: IO (F.FunPtr (IO FC.CInt))

{-# NOINLINE y_ptr #-}

y_ptr :: F.FunPtr (IO FC.CInt)
y_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_2d9291944d890d18

{-| __C declaration:__ @x1@

    __defined at:__ @fun_attributes.h:138:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_8dadf866461c7be6" x1
  :: IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @fun_attributes.h:138:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_8839814efdc23f88" hs_bindgen_test_fun_attributes_8839814efdc23f88
  :: IO (F.FunPtr (IO FC.CInt))

{-# NOINLINE x1_ptr #-}

x1_ptr :: F.FunPtr (IO FC.CInt)
x1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_8839814efdc23f88

{-| __C declaration:__ @x2@

    __defined at:__ @fun_attributes.h:141:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_31759f8ffef2c6b0" x2
  :: IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @fun_attributes.h:141:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_c63d8c58f9a27a01" hs_bindgen_test_fun_attributes_c63d8c58f9a27a01
  :: IO (F.FunPtr (IO FC.CInt))

{-# NOINLINE x2_ptr #-}

x2_ptr :: F.FunPtr (IO FC.CInt)
x2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_c63d8c58f9a27a01
