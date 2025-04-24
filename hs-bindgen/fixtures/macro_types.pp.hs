{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

newtype PtrInt = PtrInt
  { un_PtrInt :: F.Ptr FC.CInt
  }

deriving newtype instance F.Storable PtrInt

newtype PtrPtrChar = PtrPtrChar
  { un_PtrPtrChar :: F.Ptr (F.Ptr FC.CChar)
  }

deriving newtype instance F.Storable PtrPtrChar

newtype Arr1 = Arr1
  { un_Arr1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt
  }

deriving newtype instance F.Storable Arr1

newtype Arr2 = Arr2
  { un_Arr2 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) (F.Ptr FC.CFloat)
  }

deriving newtype instance F.Storable Arr2

newtype Arr3 = Arr3
  { un_Arr3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) (F.FunPtr (FC.CDouble -> IO FC.CFloat))
  }

deriving newtype instance F.Storable Arr3

newtype Fun1 = Fun1
  { un_Fun1 :: FC.CInt -> IO (F.Ptr FC.CFloat)
  }

deriving newtype instance F.Storable Fun1

newtype Fun2 = Fun2
  { un_Fun2 :: F.FunPtr (FC.CFloat -> (F.Ptr FC.CDouble) -> IO FC.CInt)
  }

deriving newtype instance F.Storable Fun2

newtype Fun3 = Fun3
  { un_Fun3 :: F.FunPtr ((F.Ptr FC.CFloat) -> IO (F.Ptr FC.CInt))
  }

deriving newtype instance F.Storable Fun3

newtype Fun4 = Fun4
  { un_Fun4 :: FC.CInt -> (F.Ptr FC.CLong) -> IO (F.FunPtr (FC.CFloat -> (F.Ptr FC.CDouble) -> IO (F.Ptr FC.CLong)))
  }

deriving newtype instance F.Storable Fun4

newtype Fun5 = Fun5
  { un_Fun5 :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 8) FC.CChar) -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) (F.Ptr FC.CShort)))
  }

deriving newtype instance F.Storable Fun5

newtype MTy = MTy
  { un_MTy :: FC.CFloat
  }

deriving newtype instance F.Storable MTy

deriving stock instance Eq MTy

deriving stock instance Ord MTy

deriving stock instance Read MTy

deriving stock instance Show MTy

deriving newtype instance Enum MTy

deriving newtype instance Floating MTy

deriving newtype instance Fractional MTy

deriving newtype instance Num MTy

deriving newtype instance Real MTy

deriving newtype instance RealFloat MTy

deriving newtype instance RealFrac MTy

newtype Tty = Tty
  { un_Tty :: MTy
  }

deriving newtype instance F.Storable Tty

newtype UINT8_T = UINT8_T
  { un_UINT8_T :: FC.CUChar
  }

deriving newtype instance F.Storable UINT8_T

deriving stock instance Eq UINT8_T

deriving stock instance Ord UINT8_T

deriving stock instance Read UINT8_T

deriving stock instance Show UINT8_T

deriving newtype instance Enum UINT8_T

deriving newtype instance Ix.Ix UINT8_T

deriving newtype instance Bounded UINT8_T

deriving newtype instance Bits.Bits UINT8_T

deriving newtype instance FiniteBits UINT8_T

deriving newtype instance Integral UINT8_T

deriving newtype instance Num UINT8_T

deriving newtype instance Real UINT8_T

newtype BOOLEAN_T = BOOLEAN_T
  { un_BOOLEAN_T :: UINT8_T
  }

deriving newtype instance F.Storable BOOLEAN_T

newtype Boolean_T = Boolean_T
  { un_Boolean_T :: BOOLEAN_T
  }

deriving newtype instance F.Storable Boolean_T
