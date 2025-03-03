{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

newtype I = I
  { unI :: FC.CInt
  }

deriving newtype instance F.Storable I

deriving stock instance Eq I

deriving stock instance Ord I

deriving stock instance Read I

deriving stock instance Show I

deriving newtype instance Enum I

deriving newtype instance Ix.Ix I

deriving newtype instance Bounded I

deriving newtype instance Bits.Bits I

deriving newtype instance FiniteBits I

deriving newtype instance Integral I

deriving newtype instance Num I

deriving newtype instance Real I

newtype C = C
  { unC :: FC.CChar
  }

deriving newtype instance F.Storable C

deriving stock instance Eq C

deriving stock instance Ord C

deriving stock instance Read C

deriving stock instance Show C

deriving newtype instance Enum C

deriving newtype instance Ix.Ix C

deriving newtype instance Bounded C

deriving newtype instance Bits.Bits C

deriving newtype instance FiniteBits C

deriving newtype instance Integral C

deriving newtype instance Num C

deriving newtype instance Real C

newtype F = F
  { unF :: FC.CFloat
  }

deriving newtype instance F.Storable F

deriving stock instance Eq F

deriving stock instance Ord F

deriving stock instance Read F

deriving stock instance Show F

deriving newtype instance Enum F

deriving newtype instance Floating F

deriving newtype instance Fractional F

deriving newtype instance Num F

deriving newtype instance Real F

deriving newtype instance RealFloat F

deriving newtype instance RealFrac F

newtype L = L
  { unL :: FC.CLong
  }

deriving newtype instance F.Storable L

deriving stock instance Eq L

deriving stock instance Ord L

deriving stock instance Read L

deriving stock instance Show L

deriving newtype instance Enum L

deriving newtype instance Ix.Ix L

deriving newtype instance Bounded L

deriving newtype instance Bits.Bits L

deriving newtype instance FiniteBits L

deriving newtype instance Integral L

deriving newtype instance Num L

deriving newtype instance Real L

newtype S = S
  { unS :: FC.CShort
  }

deriving newtype instance F.Storable S

deriving stock instance Eq S

deriving stock instance Ord S

deriving stock instance Read S

deriving stock instance Show S

deriving newtype instance Enum S

deriving newtype instance Ix.Ix S

deriving newtype instance Bounded S

deriving newtype instance Bits.Bits S

deriving newtype instance FiniteBits S

deriving newtype instance Integral S

deriving newtype instance Num S

deriving newtype instance Real S

foreign import capi safe "macro_in_fundecl.h quux" quux :: F -> FC.CChar -> IO FC.CChar

foreign import capi safe "macro_in_fundecl.h wam" wam :: FC.CFloat -> (F.Ptr C) -> IO (F.Ptr C)

foreign import capi safe "macro_in_fundecl.h foo1" foo1 :: FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar)

foreign import capi safe "macro_in_fundecl.h foo2" foo2 :: F -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr FC.CChar)

foreign import capi safe "macro_in_fundecl.h foo3" foo3 :: FC.CFloat -> (F.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (F.Ptr C)

foreign import capi safe "macro_in_fundecl.h bar1" bar1 :: FC.CLong -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import capi safe "macro_in_fundecl.h bar2" bar2 :: L -> IO (F.FunPtr (FC.CShort -> IO FC.CInt))

foreign import capi safe "macro_in_fundecl.h bar3" bar3 :: FC.CLong -> IO (F.FunPtr (S -> IO FC.CInt))

foreign import capi safe "macro_in_fundecl.h bar4" bar4 :: FC.CLong -> IO (F.FunPtr (FC.CShort -> IO I))

foreign import capi safe "macro_in_fundecl.h baz1" baz1 :: FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import capi safe "macro_in_fundecl.h baz2" baz2 :: I -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import capi safe "macro_in_fundecl.h baz3" baz3 :: FC.CInt -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))
