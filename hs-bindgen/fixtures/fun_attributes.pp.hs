{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Bits (FiniteBits)
import Data.Void (Void)
import Prelude (Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

$(HsBindgen.Runtime.Prelude.addCSource "#include <fun_attributes.h>\nvoid hs_bindgen_test_fun_attributes_8de545512324157b (void) { __f1(); }\nvoid hs_bindgen_test_fun_attributes_a2f84d2570ef3892 (void) { f1(); }\nvoid *hs_bindgen_test_fun_attributes_cefda6b95395d829 (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }\nvoid *hs_bindgen_test_fun_attributes_e25f06c3ebec2536 (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }\nvoid *hs_bindgen_test_fun_attributes_51fa664668350a00 (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }\nvoid *hs_bindgen_test_fun_attributes_93a5d6b7d4e02c33 (size_t arg1) { return my_alloc1(arg1); }\nvoid *hs_bindgen_test_fun_attributes_c948fd867be322fa (size_t arg1) { return my_alloc2(arg1); }\nsigned int hs_bindgen_test_fun_attributes_55e5eb89e54abf83 (signed int arg1) { return square(arg1); }\nsigned int hs_bindgen_test_fun_attributes_1040c24c74db8069 (void) { return old_fn(); }\nchar *hs_bindgen_test_fun_attributes_023f7813e909f518 (char *arg1, char const *arg2) { return my_dgettext(arg1, arg2); }\nFILE *hs_bindgen_test_fun_attributes_e39bbd59f1c96c14 (signed int arg1, char const *arg2) { return fdopen(arg1, arg2); }\nvoid hs_bindgen_test_fun_attributes_1d043de05a457e90 (void) { f2(); }\nvoid *hs_bindgen_test_fun_attributes_4b3bfd2d72a2db5d (void *arg1, void const *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }\nvoid hs_bindgen_test_fun_attributes_348fe595d62421cf (void) { fatal(); }\nsigned int hs_bindgen_test_fun_attributes_e30754e2591f701a (char *arg1) { return hash(arg1); }\nvoid *hs_bindgen_test_fun_attributes_f6f68a022a15937a (size_t arg1) { return mymalloc(arg1); }\nvoid hs_bindgen_test_fun_attributes_d1bf41da7ab64db1 (void) { foobar(); }\nsigned int hs_bindgen_test_fun_attributes_00405e83bcb9b271 (void) { return core2_func(); }\nsigned int hs_bindgen_test_fun_attributes_06e7d2f8bcf43684 (void) { return sse3_func(); }\nvoid hs_bindgen_test_fun_attributes_e23eff1955ebb459 (void) { f3(); }\nsigned int hs_bindgen_test_fun_attributes_ef0eea5f61ef9228 (void) { return fn(); }\nsigned int hs_bindgen_test_fun_attributes_b007466f7ff1cf28 (void) { return y(); }\nsigned int hs_bindgen_test_fun_attributes_8c9825e1b20a7ea1 (void) { return x1(); }\nsigned int hs_bindgen_test_fun_attributes_c80d61b7727dab77 (void) { return x2(); }\n/* get___f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_7003b306f73c174b (void)) (void) { return &__f1; } \n/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_5469bdc0395f86c1 (void)) (void) { return &f1; } \n/* get_my_memalign_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_b3c956e53724162c (void)) (size_t arg1, size_t arg2) { return &my_memalign; } \n/* get_my_calloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_733646ca96f39979 (void)) (size_t arg1, size_t arg2) { return &my_calloc; } \n/* get_my_realloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_94e8271f186110fd (void)) (void *arg1, size_t arg2) { return &my_realloc; } \n/* get_my_alloc1_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_48d9862d70f58e70 (void)) (size_t arg1) { return &my_alloc1; } \n/* get_my_alloc2_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_17a11fd10dc57357 (void)) (size_t arg1) { return &my_alloc2; } \n/* get_square_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_c41111f40a04cdc9 (void)) (signed int arg1) { return &square; } \n/* get_old_fn_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_3add0261fa83e1dd (void)) (void) { return &old_fn; } \n/* get_my_dgettext_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_fun_attributes_a0be4f488601c252 (void)) (char *arg1, char const *arg2) { return &my_dgettext; } \n/* get_fdopen_ptr */ __attribute__ ((const)) FILE *(*hs_bindgen_test_fun_attributes_2b987c3b5c01a326 (void)) (signed int arg1, char const *arg2) { return &fdopen; } \n/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_490ca7e8c8282a69 (void)) (void) { return &f2; } \n/* get_my_memcpy_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_e2e8b5d5ac435de8 (void)) (void *arg1, void const *arg2, size_t arg3) { return &my_memcpy; } \n/* get_fatal_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_ea0bb781f9eca7f5 (void)) (void) { return &fatal; } \n/* get_hash_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_4de9606eb9c5dd01 (void)) (char *arg1) { return &hash; } \n/* get_mymalloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_4ce141c884649d49 (void)) (size_t arg1) { return &mymalloc; } \n/* get_foobar_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_5c243ced544ab0aa (void)) (void) { return &foobar; } \n/* get_core2_func_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_14ef55245a14f816 (void)) (void) { return &core2_func; } \n/* get_sse3_func_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_72956748bb6eee67 (void)) (void) { return &sse3_func; } \n/* get_f3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_38506a9ac5626bf2 (void)) (void) { return &f3; } \n/* get_fn_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_5929da82079150d1 (void)) (void) { return &fn; } \n/* get_y_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_7bcb4a1873e6ece6 (void)) (void) { return &y; } \n/* get_x1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_11098262b345351a (void)) (void) { return &x1; } \n/* get_x2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_0d19f83087f278f9 (void)) (void) { return &x2; } \n/* get_i_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_fun_attributes_cdc30ae5fb72cd6e (void) { return &i; } \n")

{-| Attributes on functions

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

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
foreign import ccall safe "hs_bindgen_test_fun_attributes_8de545512324157b" __f1
  :: IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @fun_attributes.h:17:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_a2f84d2570ef3892" f1
  :: IO ()

{-| __C declaration:__ @my_memalign@

    __defined at:__ @fun_attributes.h:21:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_cefda6b95395d829" my_memalign
  :: Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @fun_attributes.h:26:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_e25f06c3ebec2536" my_calloc
  :: Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @fun_attributes.h:27:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_51fa664668350a00" my_realloc
  :: Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @fun_attributes.h:32:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_93a5d6b7d4e02c33" my_alloc1
  :: Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @fun_attributes.h:33:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_c948fd867be322fa" my_alloc2
  :: Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @square@

    __defined at:__ @fun_attributes.h:37:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_55e5eb89e54abf83" square
  :: FC.CInt
  -> FC.CInt

{-| __C declaration:__ @old_fn@

    __defined at:__ @fun_attributes.h:46:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_1040c24c74db8069" old_fn
  :: IO FC.CInt

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @fun_attributes.h:57:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_023f7813e909f518" my_dgettext
  :: Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @my_domain@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @my_format@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @fdopen@

    __defined at:__ @fun_attributes.h:68:9@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_e39bbd59f1c96c14" fdopen
  :: FC.CInt
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr FILE)

{-| __C declaration:__ @f2@

    __defined at:__ @fun_attributes.h:72:65@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_1d043de05a457e90" f2
  :: IO ()

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @fun_attributes.h:78:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_4b3bfd2d72a2db5d" my_memcpy
  :: Ptr.Ptr Void
     {- ^ __C declaration:__ @dest@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @src@
     -}
  -> Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @fatal@

    __defined at:__ @fun_attributes.h:95:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_348fe595d62421cf" fatal
  :: IO ()

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @fun_attributes.h:103:5@

__exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_e30754e2591f701a" hash
  :: Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @mymalloc@

    __defined at:__ @fun_attributes.h:108:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_f6f68a022a15937a" mymalloc
  :: Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @foobar@

    __defined at:__ @fun_attributes.h:112:13@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_d1bf41da7ab64db1" foobar
  :: IO ()

{-| __C declaration:__ @core2_func@

    __defined at:__ @fun_attributes.h:119:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_00405e83bcb9b271" core2_func
  :: IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @fun_attributes.h:120:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_06e7d2f8bcf43684" sse3_func
  :: IO FC.CInt

{-| __C declaration:__ @f3@

    __defined at:__ @fun_attributes.h:124:49@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_e23eff1955ebb459" f3
  :: IO ()

{-| __C declaration:__ @fn@

    __defined at:__ @fun_attributes.h:129:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_ef0eea5f61ef9228" fn
  :: IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @fun_attributes.h:135:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_b007466f7ff1cf28" y
  :: IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @fun_attributes.h:138:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_8c9825e1b20a7ea1" x1
  :: IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @fun_attributes.h:141:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_c80d61b7727dab77" x2
  :: IO FC.CInt

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_7003b306f73c174b" hs_bindgen_test_fun_attributes_7003b306f73c174b
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE __f1_ptr #-}

{-| __C declaration:__ @__f1@

    __defined at:__ @fun_attributes.h:16:6@

    __exported by:__ @fun_attributes.h@
-}
__f1_ptr :: Ptr.FunPtr (IO ())
__f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_7003b306f73c174b

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_5469bdc0395f86c1" hs_bindgen_test_fun_attributes_5469bdc0395f86c1
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

{-| __C declaration:__ @f1@

    __defined at:__ @fun_attributes.h:17:6@

    __exported by:__ @fun_attributes.h@
-}
f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_5469bdc0395f86c1

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_b3c956e53724162c" hs_bindgen_test_fun_attributes_b3c956e53724162c
  :: IO (Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memalign_ptr #-}

{-| __C declaration:__ @my_memalign@

    __defined at:__ @fun_attributes.h:21:7@

    __exported by:__ @fun_attributes.h@
-}
my_memalign_ptr :: Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void))
my_memalign_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_b3c956e53724162c

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_733646ca96f39979" hs_bindgen_test_fun_attributes_733646ca96f39979
  :: IO (Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_calloc_ptr #-}

{-| __C declaration:__ @my_calloc@

    __defined at:__ @fun_attributes.h:26:7@

    __exported by:__ @fun_attributes.h@
-}
my_calloc_ptr :: Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void))
my_calloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_733646ca96f39979

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_94e8271f186110fd" hs_bindgen_test_fun_attributes_94e8271f186110fd
  :: IO (Ptr.FunPtr ((Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_realloc_ptr #-}

{-| __C declaration:__ @my_realloc@

    __defined at:__ @fun_attributes.h:27:7@

    __exported by:__ @fun_attributes.h@
-}
my_realloc_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void))
my_realloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_94e8271f186110fd

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_48d9862d70f58e70" hs_bindgen_test_fun_attributes_48d9862d70f58e70
  :: IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_alloc1_ptr #-}

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @fun_attributes.h:32:7@

    __exported by:__ @fun_attributes.h@
-}
my_alloc1_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
my_alloc1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_48d9862d70f58e70

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_17a11fd10dc57357" hs_bindgen_test_fun_attributes_17a11fd10dc57357
  :: IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_alloc2_ptr #-}

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @fun_attributes.h:33:7@

    __exported by:__ @fun_attributes.h@
-}
my_alloc2_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
my_alloc2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_17a11fd10dc57357

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_c41111f40a04cdc9" hs_bindgen_test_fun_attributes_c41111f40a04cdc9
  :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @fun_attributes.h:37:5@

    __exported by:__ @fun_attributes.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_c41111f40a04cdc9

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_3add0261fa83e1dd" hs_bindgen_test_fun_attributes_3add0261fa83e1dd
  :: IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE old_fn_ptr #-}

{-| __C declaration:__ @old_fn@

    __defined at:__ @fun_attributes.h:46:5@

    __exported by:__ @fun_attributes.h@
-}
old_fn_ptr :: Ptr.FunPtr (IO FC.CInt)
old_fn_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_3add0261fa83e1dd

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_a0be4f488601c252" hs_bindgen_test_fun_attributes_a0be4f488601c252
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE my_dgettext_ptr #-}

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @fun_attributes.h:57:1@

    __exported by:__ @fun_attributes.h@
-}
my_dgettext_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar))
my_dgettext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_a0be4f488601c252

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_2b987c3b5c01a326" hs_bindgen_test_fun_attributes_2b987c3b5c01a326
  :: IO (Ptr.FunPtr (FC.CInt -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FILE)))

{-# NOINLINE fdopen_ptr #-}

{-| __C declaration:__ @fdopen@

    __defined at:__ @fun_attributes.h:68:9@

    __exported by:__ @fun_attributes.h@
-}
fdopen_ptr :: Ptr.FunPtr (FC.CInt -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FILE))
fdopen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_2b987c3b5c01a326

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_490ca7e8c8282a69" hs_bindgen_test_fun_attributes_490ca7e8c8282a69
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @fun_attributes.h:72:65@

    __exported by:__ @fun_attributes.h@
-}
f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_490ca7e8c8282a69

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_e2e8b5d5ac435de8" hs_bindgen_test_fun_attributes_e2e8b5d5ac435de8
  :: IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memcpy_ptr #-}

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @fun_attributes.h:78:1@

    __exported by:__ @fun_attributes.h@
-}
my_memcpy_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void))
my_memcpy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_e2e8b5d5ac435de8

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_ea0bb781f9eca7f5" hs_bindgen_test_fun_attributes_ea0bb781f9eca7f5
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE fatal_ptr #-}

{-| __C declaration:__ @fatal@

    __defined at:__ @fun_attributes.h:95:6@

    __exported by:__ @fun_attributes.h@
-}
fatal_ptr :: Ptr.FunPtr (IO ())
fatal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_ea0bb781f9eca7f5

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_4de9606eb9c5dd01" hs_bindgen_test_fun_attributes_4de9606eb9c5dd01
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE hash_ptr #-}

{-| __C declaration:__ @hash@

    __defined at:__ @fun_attributes.h:103:5@

    __exported by:__ @fun_attributes.h@
-}
hash_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
hash_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_4de9606eb9c5dd01

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_4ce141c884649d49" hs_bindgen_test_fun_attributes_4ce141c884649d49
  :: IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE mymalloc_ptr #-}

{-| __C declaration:__ @mymalloc@

    __defined at:__ @fun_attributes.h:108:1@

    __exported by:__ @fun_attributes.h@
-}
mymalloc_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
mymalloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_4ce141c884649d49

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_5c243ced544ab0aa" hs_bindgen_test_fun_attributes_5c243ced544ab0aa
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE foobar_ptr #-}

{-| __C declaration:__ @foobar@

    __defined at:__ @fun_attributes.h:112:13@

    __exported by:__ @fun_attributes.h@
-}
foobar_ptr :: Ptr.FunPtr (IO ())
foobar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_5c243ced544ab0aa

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_14ef55245a14f816" hs_bindgen_test_fun_attributes_14ef55245a14f816
  :: IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE core2_func_ptr #-}

{-| __C declaration:__ @core2_func@

    __defined at:__ @fun_attributes.h:119:5@

    __exported by:__ @fun_attributes.h@
-}
core2_func_ptr :: Ptr.FunPtr (IO FC.CInt)
core2_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_14ef55245a14f816

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_72956748bb6eee67" hs_bindgen_test_fun_attributes_72956748bb6eee67
  :: IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE sse3_func_ptr #-}

{-| __C declaration:__ @sse3_func@

    __defined at:__ @fun_attributes.h:120:5@

    __exported by:__ @fun_attributes.h@
-}
sse3_func_ptr :: Ptr.FunPtr (IO FC.CInt)
sse3_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_72956748bb6eee67

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_38506a9ac5626bf2" hs_bindgen_test_fun_attributes_38506a9ac5626bf2
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

{-| __C declaration:__ @f3@

    __defined at:__ @fun_attributes.h:124:49@

    __exported by:__ @fun_attributes.h@
-}
f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_38506a9ac5626bf2

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_5929da82079150d1" hs_bindgen_test_fun_attributes_5929da82079150d1
  :: IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE fn_ptr #-}

{-| __C declaration:__ @fn@

    __defined at:__ @fun_attributes.h:129:5@

    __exported by:__ @fun_attributes.h@
-}
fn_ptr :: Ptr.FunPtr (IO FC.CInt)
fn_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_5929da82079150d1

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_7bcb4a1873e6ece6" hs_bindgen_test_fun_attributes_7bcb4a1873e6ece6
  :: IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE y_ptr #-}

{-| __C declaration:__ @y@

    __defined at:__ @fun_attributes.h:135:12@

    __exported by:__ @fun_attributes.h@
-}
y_ptr :: Ptr.FunPtr (IO FC.CInt)
y_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_7bcb4a1873e6ece6

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_11098262b345351a" hs_bindgen_test_fun_attributes_11098262b345351a
  :: IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE x1_ptr #-}

{-| __C declaration:__ @x1@

    __defined at:__ @fun_attributes.h:138:12@

    __exported by:__ @fun_attributes.h@
-}
x1_ptr :: Ptr.FunPtr (IO FC.CInt)
x1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_11098262b345351a

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_0d19f83087f278f9" hs_bindgen_test_fun_attributes_0d19f83087f278f9
  :: IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE x2_ptr #-}

{-| __C declaration:__ @x2@

    __defined at:__ @fun_attributes.h:141:12@

    __exported by:__ @fun_attributes.h@
-}
x2_ptr :: Ptr.FunPtr (IO FC.CInt)
x2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_0d19f83087f278f9

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_cdc30ae5fb72cd6e" hs_bindgen_test_fun_attributes_cdc30ae5fb72cd6e
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i_ptr #-}

{-| __C declaration:__ @i@

    __defined at:__ @fun_attributes.h:125:5@

    __exported by:__ @fun_attributes.h@
-}
i_ptr :: Ptr.Ptr FC.CInt
i_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_cdc30ae5fb72cd6e
