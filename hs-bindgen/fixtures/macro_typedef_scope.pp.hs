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
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype T1 = T1
  { un_T1 :: FC.CInt
  }

deriving newtype instance F.Storable T1

deriving stock instance Eq T1

deriving stock instance Ord T1

deriving stock instance Read T1

deriving stock instance Show T1

deriving newtype instance Enum T1

deriving newtype instance Ix.Ix T1

deriving newtype instance Bounded T1

deriving newtype instance Bits.Bits T1

deriving newtype instance FiniteBits T1

deriving newtype instance Integral T1

deriving newtype instance Num T1

deriving newtype instance Real T1

newtype T2 = T2
  { un_T2 :: T1
  }

deriving newtype instance F.Storable T2

deriving stock instance Eq T2

deriving stock instance Ord T2

deriving stock instance Read T2

deriving stock instance Show T2

deriving newtype instance Enum T2

deriving newtype instance Ix.Ix T2

deriving newtype instance Bounded T2

deriving newtype instance Bits.Bits T2

deriving newtype instance FiniteBits T2

deriving newtype instance Integral T2

deriving newtype instance Num T2

deriving newtype instance Real T2

newtype T3 = T3
  { un_T3 :: T2
  }

deriving newtype instance F.Storable T3

deriving stock instance Eq T3

deriving stock instance Ord T3

deriving stock instance Read T3

deriving stock instance Show T3

deriving newtype instance Enum T3

deriving newtype instance Ix.Ix T3

deriving newtype instance Bounded T3

deriving newtype instance Bits.Bits T3

deriving newtype instance FiniteBits T3

deriving newtype instance Integral T3

deriving newtype instance Num T3

deriving newtype instance Real T3

newtype T4 = T4
  { un_T4 :: T3
  }

deriving newtype instance F.Storable T4

deriving stock instance Eq T4

deriving stock instance Ord T4

deriving stock instance Read T4

deriving stock instance Show T4

deriving newtype instance Enum T4

deriving newtype instance Ix.Ix T4

deriving newtype instance Bounded T4

deriving newtype instance Bits.Bits T4

deriving newtype instance FiniteBits T4

deriving newtype instance Integral T4

deriving newtype instance Num T4

deriving newtype instance Real T4
