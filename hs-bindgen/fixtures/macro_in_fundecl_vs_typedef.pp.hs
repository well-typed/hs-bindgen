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
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(CAPI.addCSource "#include <macro_in_fundecl_vs_typedef.h>\nchar hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad (MC arg1, TC arg2) { return quux1(arg1, arg2); }\n/* get_quux1_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8 (void)) (MC arg1, TC arg2) { return &quux1; } \nTC hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2 (MC arg1, char arg2) { return quux2(arg1, arg2); }\n/* get_quux2_ptr */ __attribute__ ((const)) TC (*hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b (void)) (MC arg1, char arg2) { return &quux2; } \nMC *hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0 (float arg1, TC *arg2) { return wam1(arg1, arg2); }\n/* get_wam1_ptr */ __attribute__ ((const)) MC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77 (void)) (float arg1, TC *arg2) { return &wam1; } \nTC *hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210 (float arg1, MC *arg2) { return wam2(arg1, arg2); }\n/* get_wam2_ptr */ __attribute__ ((const)) TC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f (void)) (float arg1, MC *arg2) { return &wam2; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }\n/* get_struct_typedef1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009 (void)) (struct2 *arg1, MC arg2) { return &struct_typedef1; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }\n/* get_struct_typedef2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3 (void)) (struct3_t *arg1, MC arg2) { return &struct_typedef2; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }\n/* get_struct_typedef3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802 (void)) (struct4 *arg1, MC arg2) { return &struct_typedef3; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26 (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }\n/* get_struct_name1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835 (void)) (struct struct1 *arg1, MC arg2) { return &struct_name1; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }\n/* get_struct_name2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636 (void)) (struct struct3 *arg1, MC arg2) { return &struct_name2; } \nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }\n/* get_struct_name3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788 (void)) (struct struct4 *arg1, MC arg2) { return &struct_name3; } \n")

newtype MC = MC
  { un_MC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype TC = TC
  { un_TC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __from C:__ @quux1@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad" quux1
  :: MC
     {- ^ __from C:__ @x@ -}
  -> TC
     {- ^ __from C:__ @y@ -}
  -> IO FC.CChar

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8" hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8
  :: IO (F.FunPtr (MC -> TC -> IO FC.CChar))

{-# NOINLINE quux1_ptr #-}

quux1_ptr :: F.FunPtr (MC -> TC -> IO FC.CChar)
quux1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8

{-| __from C:__ @quux2@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2" quux2
  :: MC
     {- ^ __from C:__ @x@ -}
  -> FC.CChar
     {- ^ __from C:__ @y@ -}
  -> IO TC

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b" hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b
  :: IO (F.FunPtr (MC -> FC.CChar -> IO TC))

{-# NOINLINE quux2_ptr #-}

quux2_ptr :: F.FunPtr (MC -> FC.CChar -> IO TC)
quux2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b

{-| __from C:__ @wam1@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0" wam1
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.Ptr TC
     {- ^ __from C:__ @y@ -}
  -> IO (F.Ptr MC)

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77" hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77
  :: IO (F.FunPtr (FC.CFloat -> (F.Ptr TC) -> IO (F.Ptr MC)))

{-# NOINLINE wam1_ptr #-}

wam1_ptr :: F.FunPtr (FC.CFloat -> (F.Ptr TC) -> IO (F.Ptr MC))
wam1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77

{-| __from C:__ @wam2@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210" wam2
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.Ptr MC
     {- ^ __from C:__ @y@ -}
  -> IO (F.Ptr TC)

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f" hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f
  :: IO (F.FunPtr (FC.CFloat -> (F.Ptr MC) -> IO (F.Ptr TC)))

{-# NOINLINE wam2_ptr #-}

wam2_ptr :: F.FunPtr (FC.CFloat -> (F.Ptr MC) -> IO (F.Ptr TC))
wam2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f

data Struct1 = Struct1
  { struct1_a :: FC.CInt
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

data Struct2 = Struct2
  { struct2_a :: FC.CInt
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

data Struct3 = Struct3
  { struct3_a :: FC.CInt
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

newtype Struct3_t = Struct3_t
  { un_Struct3_t :: Struct3
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

data Struct4 = Struct4
  { struct4_a :: FC.CInt
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

{-| __from C:__ @struct_typedef1@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0" struct_typedef1
  :: F.Ptr Struct2
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009" hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009
  :: IO (F.FunPtr ((F.Ptr Struct2) -> MC -> IO ()))

{-# NOINLINE struct_typedef1_ptr #-}

struct_typedef1_ptr :: F.FunPtr ((F.Ptr Struct2) -> MC -> IO ())
struct_typedef1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009

{-| __from C:__ @struct_typedef2@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d" struct_typedef2
  :: F.Ptr Struct3_t
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3" hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3
  :: IO (F.FunPtr ((F.Ptr Struct3_t) -> MC -> IO ()))

{-# NOINLINE struct_typedef2_ptr #-}

struct_typedef2_ptr :: F.FunPtr ((F.Ptr Struct3_t) -> MC -> IO ())
struct_typedef2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3

{-| __from C:__ @struct_typedef3@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94" struct_typedef3
  :: F.Ptr Struct4
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802" hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802
  :: IO (F.FunPtr ((F.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_typedef3_ptr #-}

struct_typedef3_ptr :: F.FunPtr ((F.Ptr Struct4) -> MC -> IO ())
struct_typedef3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802

{-| __from C:__ @struct_name1@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26" struct_name1
  :: F.Ptr Struct1
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835" hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835
  :: IO (F.FunPtr ((F.Ptr Struct1) -> MC -> IO ()))

{-# NOINLINE struct_name1_ptr #-}

struct_name1_ptr :: F.FunPtr ((F.Ptr Struct1) -> MC -> IO ())
struct_name1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835

{-| __from C:__ @struct_name2@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449" struct_name2
  :: F.Ptr Struct3
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636" hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636
  :: IO (F.FunPtr ((F.Ptr Struct3) -> MC -> IO ()))

{-# NOINLINE struct_name2_ptr #-}

struct_name2_ptr :: F.FunPtr ((F.Ptr Struct3) -> MC -> IO ())
struct_name2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636

{-| __from C:__ @struct_name3@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf" struct_name3
  :: F.Ptr Struct4
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788" hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788
  :: IO (F.FunPtr ((F.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_name3_ptr #-}

struct_name3_ptr :: F.FunPtr ((F.Ptr Struct4) -> MC -> IO ())
struct_name3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788
