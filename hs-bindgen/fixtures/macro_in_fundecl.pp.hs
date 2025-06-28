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
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

$(CAPI.addCSource "#include \"macro_in_fundecl.h\"\nchar testmodule_quux (F arg1, char arg2) { return quux(arg1, arg2); }\nC *testmodule_wam (float arg1, C *arg2) { return wam(arg1, arg2); }\nchar *testmodule_foo1 (float arg1, signed int (*arg2) (signed int arg1)) { return foo1(arg1, arg2); }\nchar *testmodule_foo2 (F arg1, signed int (*arg2) (signed int arg1)) { return foo2(arg1, arg2); }\nC *testmodule_foo3 (float arg1, signed int (*arg2) (signed int arg1)) { return foo3(arg1, arg2); }\nsigned int (*testmodule_bar1) (signed short arg1) (signed long arg1) { return bar1(arg1); }\nsigned int (*testmodule_bar2) (signed short arg1) (L arg1) { return bar2(arg1); }\nsigned int (*testmodule_bar3) (S arg1) (signed long arg1) { return bar3(arg1); }\nI (*testmodule_bar4) (signed short arg1) (signed long arg1) { return bar4(arg1); }\nsigned int *testmodule_baz1[2][3] (signed int arg1) { return baz1(arg1); }\nsigned int *testmodule_baz2[2][3] (I arg1) { return baz2(arg1); }\nI *testmodule_baz3[2][3] (signed int arg1) { return baz3(arg1); }\nI testmodule_no_args_no_void (void) { return no_args_no_void(); }\n")

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

foreign import ccall safe "testmodule_quux" quux :: F -> FC.CChar -> IO FC.CChar

foreign import ccall safe "testmodule_wam" wam :: FC.CFloat -> (F.Ptr C) -> IO (F.Ptr C)

foreign import ccall safe "testmodule_foo1" foo1 :: FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar)

foreign import ccall safe "testmodule_foo2" foo2 :: F -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar)

foreign import ccall safe "testmodule_foo3" foo3 :: FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr C)

foreign import ccall safe "testmodule_bar1" bar1 :: FC.CLong -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import ccall safe "testmodule_bar2" bar2 :: L -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import ccall safe "testmodule_bar3" bar3 :: FC.CLong -> IO (F.FunPtr (S -> IO FC.CInt))

foreign import ccall safe "testmodule_bar4" bar4 :: FC.CLong -> IO (F.FunPtr (FC.CShort -> IO I))

foreign import ccall safe "testmodule_baz1" baz1 :: FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "testmodule_baz2" baz2 :: I -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "testmodule_baz3" baz3 :: FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))

foreign import ccall safe "testmodule_no_args_no_void" no_args_no_void :: IO I
