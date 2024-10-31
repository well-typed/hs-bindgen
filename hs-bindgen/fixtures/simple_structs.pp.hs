{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data CS1 = MkCS1
  { cS1_a :: FC.CInt
  , cS1_b :: FC.CChar
  }

instance F.Storable CS1 where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCS1
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 32

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCS1 cS1_a2 cS1_b3 ->
               F.pokeByteOff x0 0 cS1_a2
            >> F.pokeByteOff x0 32 cS1_b3

data CS2 = MkCS2
  { cS2_a :: FC.CChar
  , cS2_b :: FC.CInt
  , cS2_c :: FC.CFloat
  }

instance F.Storable CS2 where

  sizeOf = \_ -> 12

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCS2
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 32
      <*> F.peekByteOff x0 64

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCS2 cS2_a2 cS2_b3 cS2_c4 ->
               F.pokeByteOff x0 0 cS2_a2
            >> F.pokeByteOff x0 32 cS2_b3
            >> F.pokeByteOff x0 64 cS2_c4

newtype CS2T = MkCS2T
  { unCS2T :: Void
  }

data CX = MkCX
  { cX_a :: FC.CChar
  }

instance F.Storable CX where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \x0 ->
          pure MkCX
      <*> F.peekByteOff x0 0

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCX cX_a2 -> F.pokeByteOff x0 0 cX_a2

newtype CS3T = MkCS3T
  { unCS3T :: Void
  }

data CS4 = MkCS4
  { cS4_b :: FC.CChar
  , cS4_a :: FC.CInt
  , cS4_c :: F.Ptr FC.CInt
  }

instance F.Storable CS4 where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \x0 ->
          pure MkCS4
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 32
      <*> F.peekByteOff x0 64

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCS4 cS4_b2 cS4_a3 cS4_c4 ->
               F.pokeByteOff x0 0 cS4_b2
            >> F.pokeByteOff x0 32 cS4_a3
            >> F.pokeByteOff x0 64 cS4_c4
