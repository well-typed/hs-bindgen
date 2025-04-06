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

newtype Adio'0301s = Adio'0301s
  { un_Adio'0301s :: FC.CInt
  }

deriving newtype instance F.Storable Adio'0301s

deriving stock instance Eq Adio'0301s

deriving stock instance Ord Adio'0301s

deriving stock instance Read Adio'0301s

deriving stock instance Show Adio'0301s

deriving newtype instance Enum Adio'0301s

deriving newtype instance Ix.Ix Adio'0301s

deriving newtype instance Bounded Adio'0301s

deriving newtype instance Bits.Bits Adio'0301s

deriving newtype instance FiniteBits Adio'0301s

deriving newtype instance Integral Adio'0301s

deriving newtype instance Num Adio'0301s

deriving newtype instance Real Adio'0301s
