{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
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
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

$(CAPI.addCSource "#include <macro_in_fundecl.h>\nchar hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7 (F arg1, char arg2) { return quux(arg1, arg2); }\n/* get_quux_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5 (void)) (F arg1, char arg2) { return &quux; } \nC *hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b (float arg1, C *arg2) { return wam(arg1, arg2); }\n/* get_wam_ptr */ __attribute__ ((const)) C *(*hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213 (void)) (float arg1, C *arg2) { return &wam; } \nchar *hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833 (float arg1, signed int (*arg2) (signed int arg1)) { return foo1(arg1, arg2); }\n/* get_foo1_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_macro_in_fundecl_4c474102032285f3 (void)) (float arg1, signed int (*arg2) (signed int arg1)) { return &foo1; } \nchar *hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b (F arg1, signed int (*arg2) (signed int arg1)) { return foo2(arg1, arg2); }\n/* get_foo2_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a (void)) (F arg1, signed int (*arg2) (signed int arg1)) { return &foo2; } \nC *hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07 (float arg1, signed int (*arg2) (signed int arg1)) { return foo3(arg1, arg2); }\n/* get_foo3_ptr */ __attribute__ ((const)) C *(*hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4 (void)) (float arg1, signed int (*arg2) (signed int arg1)) { return &foo3; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0 (signed long arg1)) (signed short arg1) { return bar1(arg1); }\n/* get_bar1_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1 (void)) (signed long arg1)) (signed short arg1) { return &bar1; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5 (L arg1)) (signed short arg1) { return bar2(arg1); }\n/* get_bar2_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938 (void)) (L arg1)) (signed short arg1) { return &bar2; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e (signed long arg1)) (S arg1) { return bar3(arg1); }\n/* get_bar3_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_d2fff4d202997609 (void)) (signed long arg1)) (S arg1) { return &bar3; } \nI (*hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801 (signed long arg1)) (signed short arg1) { return bar4(arg1); }\n/* get_bar4_ptr */ __attribute__ ((const)) I (*(*hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd (void)) (signed long arg1)) (signed short arg1) { return &bar4; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69 (signed int arg1))[2][3] { return baz1(arg1); }\n/* get_baz1_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_fe6e07215634608f (void)) (signed int arg1))[2][3] { return &baz1; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c (signed int arg1))[2][3] { return baz2(arg1); }\n/* get_baz2_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29 (void)) (signed int arg1))[2][3] { return &baz2; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d (signed int arg1))[2][3] { return baz3(arg1); }\n/* get_baz3_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148 (void)) (signed int arg1))[2][3] { return &baz3; } \nI hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b (void) { return no_args_no_void(); }\n/* get_no_args_no_void_ptr */ __attribute__ ((const)) I (*hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043 (void)) (void) { return &no_args_no_void; } \n")

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

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7" quux
  :: F
     {- ^ __from C:__ @x@ -}
  -> FC.CChar
     {- ^ __from C:__ @y@ -}
  -> IO FC.CChar

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5" quux_ptr
  :: F.FunPtr (F -> FC.CChar -> IO FC.CChar)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b" wam
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.Ptr C
     {- ^ __from C:__ @y@ -}
  -> IO (F.Ptr C)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213" wam_ptr
  :: F.FunPtr (FC.CFloat -> (F.Ptr C) -> IO (F.Ptr C))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833" foo1
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __from C:__ @g@ -}
  -> IO (F.Ptr FC.CChar)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_4c474102032285f3" foo1_ptr
  :: F.FunPtr (FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b" foo2
  :: F
     {- ^ __from C:__ @x@ -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __from C:__ @g@ -}
  -> IO (F.Ptr FC.CChar)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a" foo2_ptr
  :: F.FunPtr (F -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07" foo3
  :: FC.CFloat
     {- ^ __from C:__ @x@ -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __from C:__ @g@ -}
  -> IO (F.Ptr C)

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4" foo3_ptr
  :: F.FunPtr (FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr C))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0" bar1
  :: FC.CLong
     {- ^ __from C:__ @x@ -}
  -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1" bar1_ptr
  :: F.FunPtr (FC.CLong -> IO (F.FunPtr (FC.CShort -> IO FC.CInt)))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5" bar2
  :: L
  -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938" bar2_ptr
  :: F.FunPtr (L -> IO (F.FunPtr (FC.CShort -> IO FC.CInt)))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e" bar3
  :: FC.CLong
  -> IO (F.FunPtr (S -> IO FC.CInt))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_d2fff4d202997609" bar3_ptr
  :: F.FunPtr (FC.CLong -> IO (F.FunPtr (S -> IO FC.CInt)))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801" bar4
  :: FC.CLong
  -> IO (F.FunPtr (FC.CShort -> IO I))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd" bar4_ptr
  :: F.FunPtr (FC.CLong -> IO (F.FunPtr (FC.CShort -> IO I)))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69" baz1
  :: FC.CInt
     {- ^ __from C:__ @i@ -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_fe6e07215634608f" baz1_ptr
  :: F.FunPtr (FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c" baz2
  :: FC.CInt
     {- ^ __from C:__ @i@ -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29" baz2_ptr
  :: F.FunPtr (FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d" baz3
  :: FC.CInt
     {- ^ __from C:__ @i@ -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148" baz3_ptr
  :: F.FunPtr (FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b" no_args_no_void
  :: IO I

foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043" no_args_no_void_ptr
  :: F.FunPtr (IO I)
