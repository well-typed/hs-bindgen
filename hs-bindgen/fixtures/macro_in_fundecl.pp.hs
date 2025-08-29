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

{-| __/Automatically generated from C/__

    __C declaration:__ @I@

    __defined at:__ @macro_in_fundecl.h:5:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype I = I
  { un_I :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __/Automatically generated from C/__

    __C declaration:__ @C@

    __defined at:__ @macro_in_fundecl.h:6:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype C = C
  { un_C :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __/Automatically generated from C/__

    __C declaration:__ @F@

    __defined at:__ @macro_in_fundecl.h:7:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype F = F
  { un_F :: FC.CFloat
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

{-| __/Automatically generated from C/__

    __C declaration:__ @L@

    __defined at:__ @macro_in_fundecl.h:8:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype L = L
  { un_L :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __/Automatically generated from C/__

    __C declaration:__ @S@

    __defined at:__ @macro_in_fundecl.h:9:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype S = S
  { un_S :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __/Automatically generated from C/__

    __C declaration:__ @quux@

    __defined at:__ @macro_in_fundecl.h:12:6@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7" quux
  :: F
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> FC.CChar
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @y@
     -}
  -> IO FC.CChar

{-| __/Automatically generated from C/__

    __C declaration:__ @wam@

    __defined at:__ @macro_in_fundecl.h:13:4@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b" wam
  :: FC.CFloat
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.Ptr C
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @y@
     -}
  -> IO (F.Ptr C)

{-| __/Automatically generated from C/__

    __C declaration:__ @foo1@

    __defined at:__ @macro_in_fundecl.h:16:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833" foo1
  :: FC.CFloat
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @g@
     -}
  -> IO (F.Ptr FC.CChar)

{-| __/Automatically generated from C/__

    __C declaration:__ @foo2@

    __defined at:__ @macro_in_fundecl.h:17:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b" foo2
  :: F
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @g@
     -}
  -> IO (F.Ptr FC.CChar)

{-| __/Automatically generated from C/__

    __C declaration:__ @foo3@

    __defined at:__ @macro_in_fundecl.h:18:4@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07" foo3
  :: FC.CFloat
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @g@
     -}
  -> IO (F.Ptr C)

{-| __/Automatically generated from C/__

    __C declaration:__ @bar1@

    __defined at:__ @macro_in_fundecl.h:21:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0" bar1
  :: FC.CLong
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

{-| __/Automatically generated from C/__

    __C declaration:__ @bar2@

    __defined at:__ @macro_in_fundecl.h:22:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5" bar2
  :: L
  -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

{-| __/Automatically generated from C/__

    __C declaration:__ @bar3@

    __defined at:__ @macro_in_fundecl.h:23:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e" bar3
  :: FC.CLong
  -> IO (F.FunPtr (S -> IO FC.CInt))

{-| __/Automatically generated from C/__

    __C declaration:__ @bar4@

    __defined at:__ @macro_in_fundecl.h:24:5@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801" bar4
  :: FC.CLong
  -> IO (F.FunPtr (FC.CShort -> IO I))

{-| __/Automatically generated from C/__

    __C declaration:__ @baz1@

    __defined at:__ @macro_in_fundecl.h:27:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69" baz1
  :: FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @i@
     -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __/Automatically generated from C/__

    __C declaration:__ @baz2@

    __defined at:__ @macro_in_fundecl.h:35:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c" baz2
  :: FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @i@
     -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __/Automatically generated from C/__

    __C declaration:__ @baz3@

    __defined at:__ @macro_in_fundecl.h:43:5@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d" baz3
  :: FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @i@
     -}
  -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __/Automatically generated from C/__

    __C declaration:__ @no_args_no_void@

    __defined at:__ @macro_in_fundecl.h:53:3@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b" no_args_no_void
  :: IO I
