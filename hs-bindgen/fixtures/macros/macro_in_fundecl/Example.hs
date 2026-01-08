{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

{-| __C declaration:__ @I@

    __defined at:__ @macros\/macro_in_fundecl.h 5:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype I = I
  { un_I :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @C@

    __defined at:__ @macros\/macro_in_fundecl.h 6:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype C = C
  { un_C :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @F@

    __defined at:__ @macros\/macro_in_fundecl.h 7:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype F = F
  { un_F :: FC.CFloat
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

{-| __C declaration:__ @L@

    __defined at:__ @macros\/macro_in_fundecl.h 8:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype L = L
  { un_L :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @S@

    __defined at:__ @macros\/macro_in_fundecl.h 9:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype S = S
  { un_S :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

-- __unique:__ @instance ToFunPtr (FC.CShort -> IO I)@
foreign import ccall safe "wrapper" hs_bindgen_074b9de694d8f359 ::
     (FC.CShort -> IO I)
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))

-- __unique:__ @instance FromFunPtr (FC.CShort -> IO I)@
foreign import ccall safe "dynamic" hs_bindgen_c7a8adce35e64925 ::
     Ptr.FunPtr (FC.CShort -> IO I)
  -> FC.CShort -> IO I

instance HsBindgen.Runtime.FunPtr.ToFunPtr (FC.CShort -> IO I) where

  toFunPtr = hs_bindgen_074b9de694d8f359

instance HsBindgen.Runtime.FunPtr.FromFunPtr (FC.CShort -> IO I) where

  fromFunPtr = hs_bindgen_c7a8adce35e64925

-- __unique:__ @instance ToFunPtr (S -> IO FC.CInt)@
foreign import ccall safe "wrapper" hs_bindgen_ffdbafa239adf14e ::
     (S -> IO FC.CInt)
  -> IO (Ptr.FunPtr (S -> IO FC.CInt))

-- __unique:__ @instance FromFunPtr (S -> IO FC.CInt)@
foreign import ccall safe "dynamic" hs_bindgen_9c8a77fe3560cebd ::
     Ptr.FunPtr (S -> IO FC.CInt)
  -> S -> IO FC.CInt

instance HsBindgen.Runtime.FunPtr.ToFunPtr (S -> IO FC.CInt) where

  toFunPtr = hs_bindgen_ffdbafa239adf14e

instance HsBindgen.Runtime.FunPtr.FromFunPtr (S -> IO FC.CInt) where

  fromFunPtr = hs_bindgen_9c8a77fe3560cebd
