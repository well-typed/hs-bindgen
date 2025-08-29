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
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(CAPI.addCSource "#include <macro_in_fundecl_vs_typedef.h>\nchar hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad (MC arg1, TC arg2) { return quux1(arg1, arg2); }\nTC hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2 (MC arg1, char arg2) { return quux2(arg1, arg2); }\nMC *hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0 (float arg1, TC *arg2) { return wam1(arg1, arg2); }\nTC *hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210 (float arg1, MC *arg2) { return wam2(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26 (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }\n")

{-| __/Automatically generated from C/__

    __C declaration:__ @MC@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:4:9@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype MC = MC
  { un_MC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __/Automatically generated from C/__

    __C declaration:__ @TC@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:5:14@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype TC = TC
  { un_TC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __/Automatically generated from C/__

    __C declaration:__ @quux1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad" quux1
  :: MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> TC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @y@
     -}
  -> IO FC.CChar

{-| __/Automatically generated from C/__

    __C declaration:__ @quux2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2" quux2
  :: MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> FC.CChar
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @y@
     -}
  -> IO TC

{-| __/Automatically generated from C/__

    __C declaration:__ @wam1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0" wam1
  :: FC.CFloat
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.Ptr TC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @y@
     -}
  -> IO (F.Ptr MC)

{-| __/Automatically generated from C/__

    __C declaration:__ @wam2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210" wam2
  :: FC.CFloat
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.Ptr MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @y@
     -}
  -> IO (F.Ptr TC)

{-| __/Automatically generated from C/__

    __C declaration:__ @struct1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:18:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct1 = Struct1
  { struct1_a :: FC.CInt
    {- ^ __/Automatically generated from C/__

         __C declaration:__ @a@

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

{-| __/Automatically generated from C/__

    __C declaration:__ @struct2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:19:9@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct2 = Struct2
  { struct2_a :: FC.CInt
    {- ^ __/Automatically generated from C/__

         __C declaration:__ @a@

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

{-| __/Automatically generated from C/__

    __C declaration:__ @struct3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:20:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct3 = Struct3
  { struct3_a :: FC.CInt
    {- ^ __/Automatically generated from C/__

         __C declaration:__ @a@

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

{-| __/Automatically generated from C/__

    __C declaration:__ @struct3_t@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:20:35@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype Struct3_t = Struct3_t
  { un_Struct3_t :: Struct3
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __/Automatically generated from C/__

    __C declaration:__ @struct4@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:21:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct4 = Struct4
  { struct4_a :: FC.CInt
    {- ^ __/Automatically generated from C/__

         __C declaration:__ @a@

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

{-| __/Automatically generated from C/__

    __C declaration:__ @struct_typedef1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0" struct_typedef1
  :: F.Ptr Struct2
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @s@
     -}
  -> MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @struct_typedef2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d" struct_typedef2
  :: F.Ptr Struct3_t
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @s@
     -}
  -> MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @struct_typedef3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94" struct_typedef3
  :: F.Ptr Struct4
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @s@
     -}
  -> MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @struct_name1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26" struct_name1
  :: F.Ptr Struct1
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @s@
     -}
  -> MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @struct_name2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449" struct_name2
  :: F.Ptr Struct3
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @s@
     -}
  -> MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @struct_name3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf" struct_name3
  :: F.Ptr Struct4
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @s@
     -}
  -> MC
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> IO ()
