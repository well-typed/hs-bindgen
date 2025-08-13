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

$(CAPI.addCSource "#include <macro_in_fundecl.h>\nchar test_internal_quux (float arg1, char arg2) { return quux(arg1, arg2); }\nchar *test_internal_wam (float arg1, char *arg2) { return wam(arg1, arg2); }\nchar *test_internal_foo1 (float arg1, signed int (*arg2) (signed int arg1)) { return foo1(arg1, arg2); }\nchar *test_internal_foo2 (float arg1, signed int (*arg2) (signed int arg1)) { return foo2(arg1, arg2); }\nchar *test_internal_foo3 (float arg1, signed int (*arg2) (signed int arg1)) { return foo3(arg1, arg2); }\nsigned int (*test_internal_bar1 (signed long arg1)) (signed short arg1) { return bar1(arg1); }\nsigned int (*test_internal_bar2 (signed long arg1)) (signed short arg1) { return bar2(arg1); }\nsigned int (*test_internal_bar3 (signed long arg1)) (signed short arg1) { return bar3(arg1); }\nsigned int (*test_internal_bar4 (signed long arg1)) (signed short arg1) { return bar4(arg1); }\nsigned int (*test_internal_baz1 (signed int arg1))[2][3] { return baz1(arg1); }\nsigned int (*test_internal_baz2 (signed int arg1))[2][3] { return baz2(arg1); }\nsigned int (*test_internal_baz3 (signed int arg1))[2][3] { return baz3(arg1); }\nI test_internal_no_args_no_void (void) { return no_args_no_void(); }\n")

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

foreign import ccall safe "test_internal_quux" quux :: FC.CFloat -> FC.CChar -> IO FC.CChar

foreign import ccall safe "test_internal_wam" wam :: FC.CFloat -> (F.Ptr FC.CChar) -> IO (F.Ptr FC.CChar)

foreign import ccall safe "test_internal_foo1" foo1 :: FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar)

foreign import ccall safe "test_internal_foo2" foo2 :: FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar)

foreign import ccall safe "test_internal_foo3" foo3 :: FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar)

foreign import ccall safe "test_internal_bar1" bar1 :: FC.CLong -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import ccall safe "test_internal_bar2" bar2 :: FC.CLong -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import ccall safe "test_internal_bar3" bar3 :: FC.CLong -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import ccall safe "test_internal_bar4" bar4 :: FC.CLong -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import ccall safe "test_internal_baz1" baz1 :: FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "test_internal_baz2" baz2 :: FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "test_internal_baz3" baz3 :: FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "test_internal_no_args_no_void" no_args_no_void :: IO I
