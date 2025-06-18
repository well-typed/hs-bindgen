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
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

newtype MY_TYPE = MY_TYPE
  { un_MY_TYPE :: FC.CInt
  }

deriving newtype instance F.Storable MY_TYPE

deriving stock instance Eq MY_TYPE

deriving stock instance Ord MY_TYPE

deriving stock instance Read MY_TYPE

deriving stock instance Show MY_TYPE

deriving newtype instance Enum MY_TYPE

deriving newtype instance Ix.Ix MY_TYPE

deriving newtype instance Bounded MY_TYPE

deriving newtype instance Bits.Bits MY_TYPE

deriving newtype instance FiniteBits MY_TYPE

deriving newtype instance Integral MY_TYPE

deriving newtype instance Num MY_TYPE

deriving newtype instance Real MY_TYPE

data Bar = Bar
  { bar_x :: FC.CInt
  , bar_y :: MY_TYPE
  }

instance F.Storable Bar where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 bar_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) bar_x2
            >> F.pokeByteOff ptr0 (4 :: Int) bar_y3

deriving stock instance Show Bar

deriving stock instance Eq Bar
