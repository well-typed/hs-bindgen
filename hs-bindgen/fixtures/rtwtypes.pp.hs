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

newtype UINT8_T = UINT8_T
  { un_UINT8_T :: FC.CSChar
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

newtype Boolean_T = Boolean_T
  { un_Boolean_T :: FC.CSChar
  }

deriving newtype instance F.Storable Boolean_T

deriving stock instance Eq Boolean_T

deriving stock instance Ord Boolean_T

deriving stock instance Read Boolean_T

deriving stock instance Show Boolean_T

deriving newtype instance Enum Boolean_T

deriving newtype instance Ix.Ix Boolean_T

deriving newtype instance Bounded Boolean_T

deriving newtype instance Bits.Bits Boolean_T

deriving newtype instance FiniteBits Boolean_T

deriving newtype instance Integral Boolean_T

deriving newtype instance Num Boolean_T

deriving newtype instance Real Boolean_T
