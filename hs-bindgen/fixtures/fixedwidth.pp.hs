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
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

newtype Uint64_t = Uint64_t
  { un_Uint64_t :: FC.CULong
  }

deriving newtype instance F.Storable Uint64_t

deriving stock instance Eq Uint64_t

deriving stock instance Ord Uint64_t

deriving stock instance Read Uint64_t

deriving stock instance Show Uint64_t

deriving newtype instance Enum Uint64_t

deriving newtype instance Ix.Ix Uint64_t

deriving newtype instance Bounded Uint64_t

deriving newtype instance Bits.Bits Uint64_t

deriving newtype instance FiniteBits Uint64_t

deriving newtype instance Integral Uint64_t

deriving newtype instance Num Uint64_t

deriving newtype instance Real Uint64_t

newtype Uint32_t = Uint32_t
  { un_Uint32_t :: FC.CUInt
  }

deriving newtype instance F.Storable Uint32_t

deriving stock instance Eq Uint32_t

deriving stock instance Ord Uint32_t

deriving stock instance Read Uint32_t

deriving stock instance Show Uint32_t

deriving newtype instance Enum Uint32_t

deriving newtype instance Ix.Ix Uint32_t

deriving newtype instance Bounded Uint32_t

deriving newtype instance Bits.Bits Uint32_t

deriving newtype instance FiniteBits Uint32_t

deriving newtype instance Integral Uint32_t

deriving newtype instance Num Uint32_t

deriving newtype instance Real Uint32_t

data Foo = Foo
  { foo_sixty_four :: Uint64_t
  , foo_thirty_two :: Uint32_t
  }

instance F.Storable Foo where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               F.pokeByteOff ptr0 (0 :: Int) foo_sixty_four2
            >> F.pokeByteOff ptr0 (8 :: Int) foo_thirty_two3

deriving stock instance Show Foo

deriving stock instance Eq Foo
