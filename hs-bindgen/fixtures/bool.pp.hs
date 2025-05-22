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

data Bools1 = Bools1
  { bools1_x :: FC.CBool
  , bools1_y :: FC.CBool
  }

instance F.Storable Bools1 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools1
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools1 bools1_x2 bools1_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) bools1_x2
            >> F.pokeByteOff ptr0 (1 :: Int) bools1_y3

deriving stock instance Show Bools1

deriving stock instance Eq Bools1

data Bools2 = Bools2
  { bools2_x :: FC.CBool
  , bools2_y :: FC.CBool
  }

instance F.Storable Bools2 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools2
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools2 bools2_x2 bools2_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) bools2_x2
            >> F.pokeByteOff ptr0 (1 :: Int) bools2_y3

deriving stock instance Show Bools2

deriving stock instance Eq Bools2

newtype BOOL = BOOL
  { un_BOOL :: FC.CBool
  }

deriving newtype instance F.Storable BOOL

deriving stock instance Eq BOOL

deriving stock instance Ord BOOL

deriving stock instance Read BOOL

deriving stock instance Show BOOL

deriving newtype instance Enum BOOL

deriving newtype instance Ix.Ix BOOL

deriving newtype instance Bounded BOOL

deriving newtype instance Bits.Bits BOOL

deriving newtype instance FiniteBits BOOL

deriving newtype instance Integral BOOL

deriving newtype instance Num BOOL

deriving newtype instance Real BOOL

data Bools3 = Bools3
  { bools3_x :: BOOL
  , bools3_y :: BOOL
  }

instance F.Storable Bools3 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools3
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools3 bools3_x2 bools3_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) bools3_x2
            >> F.pokeByteOff ptr0 (1 :: Int) bools3_y3

deriving stock instance Show Bools3

deriving stock instance Eq Bools3
