{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(CAPI.addCSource "#include <macro_in_fundecl_vs_typedef.h>\nchar hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad (MC arg1, TC arg2) { return quux1(arg1, arg2); }\n/* get_quux1_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8 (void)) (MC arg1, TC arg2) { return &quux1; } \nTC hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2 (MC arg1, char arg2) { return quux2(arg1, arg2); }\n/* get_quux2_ptr */ __attribute__ ((const)) TC (*hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b (void)) (MC arg1, char arg2) { return &quux2; } \nMC *hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0 (float arg1, TC *arg2) { return wam1(arg1, arg2); }\n/* get_wam1_ptr */ __attribute__ ((const)) MC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77 (void)) (float arg1, TC *arg2) { return &wam1; } \nTC *hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210 (float arg1, MC *arg2) { return wam2(arg1, arg2); }\n/* get_wam2_ptr */ __attribute__ ((const)) TC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f (void)) (float arg1, MC *arg2) { return &wam2; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }\n/* get_struct_typedef1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009 (void)) (struct2 *arg1, MC arg2) { return &struct_typedef1; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }\n/* get_struct_typedef2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3 (void)) (struct3_t *arg1, MC arg2) { return &struct_typedef2; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }\n/* get_struct_typedef3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802 (void)) (struct4 *arg1, MC arg2) { return &struct_typedef3; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26 (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }\n/* get_struct_name1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835 (void)) (struct struct1 *arg1, MC arg2) { return &struct_name1; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }\n/* get_struct_name2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636 (void)) (struct struct3 *arg1, MC arg2) { return &struct_name2; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }\n/* get_struct_name3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788 (void)) (struct struct4 *arg1, MC arg2) { return &struct_name3; } \n")

{-| __C declaration:__ @MC@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:4:9@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype MC = MC
  { un_MC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @TC@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:5:14@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype TC = TC
  { un_TC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @quux1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad" quux1
  :: MC
     {- ^ __C declaration:__ @x@
     -}
  -> TC
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @quux1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8" hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8
  :: IO (Ptr.FunPtr (MC -> TC -> IO FC.CChar))

{-# NOINLINE quux1_ptr #-}

quux1_ptr :: Ptr.FunPtr (MC -> TC -> IO FC.CChar)
quux1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8

{-| __C declaration:__ @quux2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2" quux2
  :: MC
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @y@
     -}
  -> IO TC

{-| __C declaration:__ @quux2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b" hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b
  :: IO (Ptr.FunPtr (MC -> FC.CChar -> IO TC))

{-# NOINLINE quux2_ptr #-}

quux2_ptr :: Ptr.FunPtr (MC -> FC.CChar -> IO TC)
quux2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b

{-| __C declaration:__ @wam1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0" wam1
  :: FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr TC
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr MC)

{-| __C declaration:__ @wam1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77" hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77
  :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC)))

{-# NOINLINE wam1_ptr #-}

wam1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC))
wam1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77

{-| __C declaration:__ @wam2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210" wam2
  :: FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr MC
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr TC)

{-| __C declaration:__ @wam2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f" hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f
  :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC)))

{-# NOINLINE wam2_ptr #-}

wam2_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC))
wam2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f

{-| __C declaration:__ @struct1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:18:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct1 = Struct1
  { struct1_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:18:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct1 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct1
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 struct1_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct1_a2

{-| __C declaration:__ @struct2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:19:9@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct2 = Struct2
  { struct2_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:19:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct2
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct2_a2

{-| __C declaration:__ @struct3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:20:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct3 = Struct3
  { struct3_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:20:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct3 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct3
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 struct3_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct3_a2

{-| __C declaration:__ @struct3_t@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:20:35@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype Struct3_t = Struct3_t
  { un_Struct3_t :: Struct3
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct4@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:21:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct4 = Struct4
  { struct4_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:21:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct4 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct4
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 struct4_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct4_a2

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0" struct_typedef1
  :: Ptr.Ptr Struct2
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009" hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ()))

{-# NOINLINE struct_typedef1_ptr #-}

struct_typedef1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ())
struct_typedef1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d" struct_typedef2
  :: Ptr.Ptr Struct3_t
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3" hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ()))

{-# NOINLINE struct_typedef2_ptr #-}

struct_typedef2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ())
struct_typedef2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94" struct_typedef3
  :: Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802" hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_typedef3_ptr #-}

struct_typedef3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_typedef3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26" struct_name1
  :: Ptr.Ptr Struct1
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835" hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ()))

{-# NOINLINE struct_name1_ptr #-}

struct_name1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ())
struct_name1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449" struct_name2
  :: Ptr.Ptr Struct3
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636" hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ()))

{-# NOINLINE struct_name2_ptr #-}

struct_name2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ())
struct_name2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf" struct_name3
  :: Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788" hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_name3_ptr #-}

struct_name3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_name3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788
