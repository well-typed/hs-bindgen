{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Show, pure, return)

data Flags = Flags
  { flags_fieldX :: FC.CChar
  , flags_fieldY :: FC.CChar
  }

instance F.Storable Flags where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Flags
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Flags flags_fieldX2 flags_fieldY3 ->
               F.pokeByteOff ptr0 (0 :: Int) flags_fieldX2
            >> F.pokeByteOff ptr0 (2 :: Int) flags_fieldY3

deriving stock instance Show Flags

deriving stock instance Eq Flags

data Overflow32 = Overflow32
  {}

instance F.Storable Overflow32 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek = \ptr0 -> pure Overflow32

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32 -> return ()

deriving stock instance Show Overflow32

deriving stock instance Eq Overflow32

data Overflow32b = Overflow32b
  {}

instance F.Storable Overflow32b where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek = \ptr0 -> pure Overflow32b

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32b -> return ()

deriving stock instance Show Overflow32b

deriving stock instance Eq Overflow32b

data Overflow32c = Overflow32c
  {}

instance F.Storable Overflow32c where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek = \ptr0 -> pure Overflow32c

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32c -> return ()

deriving stock instance Show Overflow32c

deriving stock instance Eq Overflow32c

data Overflow64 = Overflow64
  {}

instance F.Storable Overflow64 where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek = \ptr0 -> pure Overflow64

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow64 -> return ()

deriving stock instance Show Overflow64

deriving stock instance Eq Overflow64

data AlignA = AlignA
  {}

instance F.Storable AlignA where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek = \ptr0 -> pure AlignA

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignA -> return ()

deriving stock instance Show AlignA

deriving stock instance Eq AlignA

data AlignB = AlignB
  {}

instance F.Storable AlignB where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek = \ptr0 -> pure AlignB

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignB -> return ()

deriving stock instance Show AlignB

deriving stock instance Eq AlignB
