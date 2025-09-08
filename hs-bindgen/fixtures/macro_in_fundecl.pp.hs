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
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

$(CAPI.addCSource "#include <macro_in_fundecl.h>\nchar hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7 (F arg1, char arg2) { return quux(arg1, arg2); }\n/* get_quux_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5 (void)) (F arg1, char arg2) { return &quux; } \nC *hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b (float arg1, C *arg2) { return wam(arg1, arg2); }\n/* get_wam_ptr */ __attribute__ ((const)) C *(*hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213 (void)) (float arg1, C *arg2) { return &wam; } \nchar *hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833 (float arg1, signed int (*arg2) (signed int arg1)) { return foo1(arg1, arg2); }\n/* get_foo1_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_macro_in_fundecl_4c474102032285f3 (void)) (float arg1, signed int (*arg2) (signed int arg1)) { return &foo1; } \nchar *hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b (F arg1, signed int (*arg2) (signed int arg1)) { return foo2(arg1, arg2); }\n/* get_foo2_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a (void)) (F arg1, signed int (*arg2) (signed int arg1)) { return &foo2; } \nC *hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07 (float arg1, signed int (*arg2) (signed int arg1)) { return foo3(arg1, arg2); }\n/* get_foo3_ptr */ __attribute__ ((const)) C *(*hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4 (void)) (float arg1, signed int (*arg2) (signed int arg1)) { return &foo3; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0 (signed long arg1)) (signed short arg1) { return bar1(arg1); }\n/* get_bar1_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1 (void)) (signed long arg1)) (signed short arg1) { return &bar1; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5 (L arg1)) (signed short arg1) { return bar2(arg1); }\n/* get_bar2_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938 (void)) (L arg1)) (signed short arg1) { return &bar2; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e (signed long arg1)) (S arg1) { return bar3(arg1); }\n/* get_bar3_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_d2fff4d202997609 (void)) (signed long arg1)) (S arg1) { return &bar3; } \nI (*hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801 (signed long arg1)) (signed short arg1) { return bar4(arg1); }\n/* get_bar4_ptr */ __attribute__ ((const)) I (*(*hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd (void)) (signed long arg1)) (signed short arg1) { return &bar4; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69 (signed int const arg1))[2][3] { return baz1(arg1); }\n/* get_baz1_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_fe6e07215634608f (void)) (signed int const arg1))[2][3] { return &baz1; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c (signed int const arg1))[2][3] { return baz2(arg1); }\n/* get_baz2_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29 (void)) (signed int const arg1))[2][3] { return &baz2; } \nsigned int (*hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d (signed int const arg1))[2][3] { return baz3(arg1); }\n/* get_baz3_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148 (void)) (signed int const arg1))[2][3] { return &baz3; } \nI hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b (void) { return no_args_no_void(); }\n/* get_no_args_no_void_ptr */ __attribute__ ((const)) I (*hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043 (void)) (void) { return &no_args_no_void; } \n")

{-| __C declaration:__ @I@

    __defined at:__ @macro_in_fundecl.h:5:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype I = I
  { un_I :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @C@

    __defined at:__ @macro_in_fundecl.h:6:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype C = C
  { un_C :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @F@

    __defined at:__ @macro_in_fundecl.h:7:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype F = F
  { un_F :: FC.CFloat
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

{-| __C declaration:__ @L@

    __defined at:__ @macro_in_fundecl.h:8:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype L = L
  { un_L :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @S@

    __defined at:__ @macro_in_fundecl.h:9:9@

    __exported by:__ @macro_in_fundecl.h@
-}
newtype S = S
  { un_S :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @quux@

    __defined at:__ @macro_in_fundecl.h:12:6@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7" quux
  :: F
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @quux@

    __defined at:__ @macro_in_fundecl.h:12:6@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5" hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5
  :: IO (Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar))

{-# NOINLINE quux_ptr #-}

quux_ptr :: Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar)
quux_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5

{-| __C declaration:__ @wam@

    __defined at:__ @macro_in_fundecl.h:13:4@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b" wam
  :: FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr C
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr C)

{-| __C declaration:__ @wam@

    __defined at:__ @macro_in_fundecl.h:13:4@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213" hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213
  :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C)))

{-# NOINLINE wam_ptr #-}

wam_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C))
wam_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213

{-| __C declaration:__ @foo1@

    __defined at:__ @macro_in_fundecl.h:16:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833" foo1
  :: FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @g@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @foo1@

    __defined at:__ @macro_in_fundecl.h:16:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_4c474102032285f3" hs_bindgen_test_macro_in_fundecl_4c474102032285f3
  :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE foo1_ptr #-}

foo1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_4c474102032285f3

{-| __C declaration:__ @foo2@

    __defined at:__ @macro_in_fundecl.h:17:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b" foo2
  :: F
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @g@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @foo2@

    __defined at:__ @macro_in_fundecl.h:17:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a" hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a
  :: IO (Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE foo2_ptr #-}

foo2_ptr :: Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a

{-| __C declaration:__ @foo3@

    __defined at:__ @macro_in_fundecl.h:18:4@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07" foo3
  :: FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @g@
     -}
  -> IO (Ptr.Ptr C)

{-| __C declaration:__ @foo3@

    __defined at:__ @macro_in_fundecl.h:18:4@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4" hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4
  :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C)))

{-# NOINLINE foo3_ptr #-}

foo3_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C))
foo3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4

{-| __C declaration:__ @bar1@

    __defined at:__ @macro_in_fundecl.h:21:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0" bar1
  :: FC.CLong
     {- ^ __C declaration:__ @x@
     -}
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))

{-| __C declaration:__ @bar1@

    __defined at:__ @macro_in_fundecl.h:21:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1" hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1
  :: IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))

{-# NOINLINE bar1_ptr #-}

bar1_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1

{-| __C declaration:__ @bar2@

    __defined at:__ @macro_in_fundecl.h:22:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5" bar2
  :: L
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))

{-| __C declaration:__ @bar2@

    __defined at:__ @macro_in_fundecl.h:22:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938" hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938
  :: IO (Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))

{-# NOINLINE bar2_ptr #-}

bar2_ptr :: Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938

{-| __C declaration:__ @bar3@

    __defined at:__ @macro_in_fundecl.h:23:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e" bar3
  :: FC.CLong
  -> IO (Ptr.FunPtr (S -> IO FC.CInt))

{-| __C declaration:__ @bar3@

    __defined at:__ @macro_in_fundecl.h:23:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_d2fff4d202997609" hs_bindgen_test_macro_in_fundecl_d2fff4d202997609
  :: IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt))))

{-# NOINLINE bar3_ptr #-}

bar3_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt)))
bar3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_d2fff4d202997609

{-| __C declaration:__ @bar4@

    __defined at:__ @macro_in_fundecl.h:24:5@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801" bar4
  :: FC.CLong
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))

{-| __C declaration:__ @bar4@

    __defined at:__ @macro_in_fundecl.h:24:5@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd" hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd
  :: IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I))))

{-# NOINLINE bar4_ptr #-}

bar4_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I)))
bar4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd

{-| __C declaration:__ @baz1@

    __defined at:__ @macro_in_fundecl.h:27:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69" baz1
  :: FC.CInt
     {- ^ __C declaration:__ @i@
     -}
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __C declaration:__ @baz1@

    __defined at:__ @macro_in_fundecl.h:27:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_fe6e07215634608f" hs_bindgen_test_macro_in_fundecl_fe6e07215634608f
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE baz1_ptr #-}

baz1_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_fe6e07215634608f

{-| __C declaration:__ @baz2@

    __defined at:__ @macro_in_fundecl.h:35:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c" baz2
  :: FC.CInt
     {- ^ __C declaration:__ @i@
     -}
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __C declaration:__ @baz2@

    __defined at:__ @macro_in_fundecl.h:35:7@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29" hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE baz2_ptr #-}

baz2_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29

{-| __C declaration:__ @baz3@

    __defined at:__ @macro_in_fundecl.h:43:5@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d" baz3
  :: FC.CInt
     {- ^ __C declaration:__ @i@
     -}
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __C declaration:__ @baz3@

    __defined at:__ @macro_in_fundecl.h:43:5@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148" hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE baz3_ptr #-}

baz3_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macro_in_fundecl.h:53:3@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b" no_args_no_void
  :: IO I

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macro_in_fundecl.h:53:3@

    __exported by:__ @macro_in_fundecl.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043" hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043
  :: IO (Ptr.FunPtr (IO I))

{-# NOINLINE no_args_no_void_ptr #-}

no_args_no_void_ptr :: Ptr.FunPtr (IO I)
no_args_no_void_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043
