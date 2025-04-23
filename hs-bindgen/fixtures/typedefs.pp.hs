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
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype Myint = Myint
  { un_Myint :: FC.CInt
  }

deriving newtype instance F.Storable Myint

deriving stock instance Eq Myint

deriving stock instance Ord Myint

deriving stock instance Read Myint

deriving stock instance Show Myint

deriving newtype instance Enum Myint

deriving newtype instance Ix.Ix Myint

deriving newtype instance Bounded Myint

deriving newtype instance Bits.Bits Myint

deriving newtype instance FiniteBits Myint

deriving newtype instance Integral Myint

deriving newtype instance Num Myint

deriving newtype instance Real Myint

newtype Intptr = Intptr
  { un_Intptr :: F.Ptr FC.CInt
  }

deriving newtype instance F.Storable Intptr

deriving stock instance Eq Intptr

deriving stock instance Ord Intptr

deriving stock instance Show Intptr
