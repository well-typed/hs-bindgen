{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure, return)

data Flags = Flags
  { flags_fieldX :: FC.CChar
  , flags_fieldY :: FC.CChar
  }

instance F.Storable Flags where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Flags
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 2

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Flags flags_fieldX2 flags_fieldY3 ->
               F.pokeByteOff ptr0 0 flags_fieldX2
            >> F.pokeByteOff ptr0 2 flags_fieldY3

data Overflow32 = Overflow32
  {}

instance F.Storable Overflow32 where

  sizeOf = \_ -> 12

  alignment = \_ -> 4

  peek = \ptr0 -> pure Overflow32

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32 -> return (())

data Overflow32b = Overflow32b
  {}

instance F.Storable Overflow32b where

  sizeOf = \_ -> 8

  alignment = \_ -> 8

  peek = \ptr0 -> pure Overflow32b

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32b -> return (())

data Overflow32c = Overflow32c
  {}

instance F.Storable Overflow32c where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek = \ptr0 -> pure Overflow32c

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32c -> return (())

data Overflow64 = Overflow64
  {}

instance F.Storable Overflow64 where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek = \ptr0 -> pure Overflow64

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow64 -> return (())

data AlignA = AlignA
  {}

instance F.Storable AlignA where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek = \ptr0 -> pure AlignA

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignA -> return (())

data AlignB = AlignB
  {}

instance F.Storable AlignB where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek = \ptr0 -> pure AlignB

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignB -> return (())
