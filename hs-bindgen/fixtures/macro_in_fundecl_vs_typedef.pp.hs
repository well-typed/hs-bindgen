{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-foreign-imports #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
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

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad" quux1
  :: MC
     {- ^ __from C:__ @x@ -}
  -> TC
     {- ^ __from C:__ @y@ -}
  -> IO FC.CChar

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_c5b48c28b2fe82e8" quux1_ptr
  :: F.FunPtr (MC -> TC -> IO FC.CChar)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2" quux2
  :: MC
     {- ^ __from C:__ @x@ -}
  -> FC.CChar
     {- ^ __from C:__ @y@ -}
  -> IO TC

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_f16957714a069f3b" quux2_ptr
  :: F.FunPtr (MC -> FC.CChar -> IO TC)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0" wam1
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.Ptr TC
     {- ^ __from C:__ @y@ -}
  -> IO (F.Ptr MC)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_d6bcc35669bacd77" wam1_ptr
  :: F.FunPtr (FC.CFloat -> (F.Ptr TC) -> IO (F.Ptr MC))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210" wam2
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.Ptr MC
     {- ^ __from C:__ @y@ -}
  -> IO (F.Ptr TC)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_134ac41aded5511f" wam2_ptr
  :: F.FunPtr (FC.CFloat -> (F.Ptr MC) -> IO (F.Ptr TC))

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

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0" struct_typedef1
  :: F.Ptr Struct2
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_68ab150f99707009" struct_typedef1_ptr
  :: F.FunPtr ((F.Ptr Struct2) -> MC -> IO ())

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d" struct_typedef2
  :: F.Ptr Struct3_t
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_b031fbdf6da5bfb3" struct_typedef2_ptr
  :: F.FunPtr ((F.Ptr Struct3_t) -> MC -> IO ())

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94" struct_typedef3
  :: F.Ptr Struct4
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_5a8d8a53b2ab3802" struct_typedef3_ptr
  :: F.FunPtr ((F.Ptr Struct4) -> MC -> IO ())

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26" struct_name1
  :: F.Ptr Struct1
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_ef2f629cf616f835" struct_name1_ptr
  :: F.FunPtr ((F.Ptr Struct1) -> MC -> IO ())

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449" struct_name2
  :: F.Ptr Struct3
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_1823618a49e45636" struct_name2_ptr
  :: F.FunPtr ((F.Ptr Struct3) -> MC -> IO ())

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf" struct_name3
  :: F.Ptr Struct4
     {- ^ __from C:__ @s@ -}
  -> MC
     {- ^ __from C:__ @x@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_2437573379090788" struct_name3_ptr
  :: F.FunPtr ((F.Ptr Struct4) -> MC -> IO ())
