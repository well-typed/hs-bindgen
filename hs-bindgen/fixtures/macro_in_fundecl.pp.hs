{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
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
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

$(CAPI.addCSource "#include <macro_in_fundecl.h>\nchar hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7 (F arg1, char arg2) { return quux(arg1, arg2); }\nC *hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b (float arg1, C *arg2) { return wam(arg1, arg2); }\nchar *hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833 (float arg1, signed int (*arg2) (signed int arg1)) { return foo1(arg1, arg2); }\nchar *hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b (F arg1, signed int (*arg2) (signed int arg1)) { return foo2(arg1, arg2); }\nC *hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07 (float arg1, signed int (*arg2) (signed int arg1)) { return foo3(arg1, arg2); }\nsigned int (*hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0 (signed long arg1)) (signed short arg1) { return bar1(arg1); }\nsigned int (*hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5 (L arg1)) (signed short arg1) { return bar2(arg1); }\nsigned int (*hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e (signed long arg1)) (S arg1) { return bar3(arg1); }\nI (*hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801 (signed long arg1)) (signed short arg1) { return bar4(arg1); }\nsigned int (*hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69 (signed int arg1))[2][3] { return baz1(arg1); }\nsigned int (*hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c (signed int arg1))[2][3] { return baz2(arg1); }\nsigned int (*hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d (signed int arg1))[2][3] { return baz3(arg1); }\nI hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b (void) { return no_args_no_void(); }\n")

newtype I = I
  { un_I :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype C = C
  { un_C :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype F = F
  { un_F :: FC.CFloat
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

newtype L = L
  { un_L :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype S = S
  { un_S :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __from C:__ @quux@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7" quux
  :: F
     {- ^ __from C:__ @x@ -}
  -> FC.CChar
     {- ^ __from C:__ @y@ -}
  -> IO FC.CChar

{-| __from C:__ @wam@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b" wam
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.Ptr C
     {- ^ __from C:__ @y@ -}
  -> IO (F.Ptr C)

{-| __from C:__ @foo1@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833" foo1
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __from C:__ @g@ -}
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @foo2@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b" foo2
  :: F
     {- ^ __from C:__ @x@ -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __from C:__ @g@ -}
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @foo3@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07" foo3
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __from C:__ @g@ -}
  -> IO (F.Ptr C)

{-| __from C:__ @bar1@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0" bar1
  :: FC.CLong
     {- ^ __from C:__ @x@ -}
  -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

{-| __from C:__ @bar2@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5" bar2
  :: L
  -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

{-| __from C:__ @bar3@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e" bar3
  :: FC.CLong
  -> IO (F.FunPtr (S -> IO FC.CInt))

{-| __from C:__ @bar4@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801" bar4
  :: FC.CLong
  -> IO (F.FunPtr (FC.CShort -> IO I))

{-| __from C:__ @baz1@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69" baz1
  :: FC.CInt
     {- ^ __from C:__ @i@ -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __from C:__ @baz2@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c" baz2
  :: FC.CInt
     {- ^ __from C:__ @i@ -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __from C:__ @baz3@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d" baz3
  :: FC.CInt
     {- ^ __from C:__ @i@ -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __from C:__ @no_args_no_void@ -}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b" no_args_no_void
  :: IO I
