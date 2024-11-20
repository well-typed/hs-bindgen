{-# LANGUAGE NoImplicitPrelude #-}

module Example where

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
    \ptr0 ->
          pure MkCS1
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS1 cS1_a2 cS1_b3 ->
               F.pokeByteOff ptr0 0 cS1_a2
            >> F.pokeByteOff ptr0 4 cS1_b3

data CS2 = MkCS2
  { cS2_a :: FC.CChar
  , cS2_b :: FC.CInt
  , cS2_c :: FC.CFloat
  }

instance F.Storable CS2 where

  sizeOf = \_ -> 12

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS2
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS2 cS2_a2 cS2_b3 cS2_c4 ->
               F.pokeByteOff ptr0 0 cS2_a2
            >> F.pokeByteOff ptr0 4 cS2_b3
            >> F.pokeByteOff ptr0 8 cS2_c4

newtype CS2T = MkCS2T
  { unCS2T :: CS2
  }

deriving newtype instance F.Storable CS2T

data CS3T = MkCS3T
  { cS3T_a :: FC.CChar
  }

instance F.Storable CS3T where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure MkCS3T
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS3T cS3T_a2 -> F.pokeByteOff ptr0 0 cS3T_a2

data CS4 = MkCS4
  { cS4_b :: FC.CChar
  , cS4_a :: FC.CInt
  , cS4_c :: F.Ptr FC.CInt
  }

instance F.Storable CS4 where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure MkCS4
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS4 cS4_b2 cS4_a3 cS4_c4 ->
               F.pokeByteOff ptr0 0 cS4_b2
            >> F.pokeByteOff ptr0 4 cS4_a3
            >> F.pokeByteOff ptr0 8 cS4_c4

data CS5 = MkCS5
  { cS5_a :: FC.CChar
  , cS5_b :: FC.CInt
  }

instance F.Storable CS5 where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS5
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS5 cS5_a2 cS5_b3 ->
               F.pokeByteOff ptr0 0 cS5_a2
            >> F.pokeByteOff ptr0 4 cS5_b3

data CS6 = MkCS6
  { cS6_a :: FC.CChar
  , cS6_b :: FC.CInt
  }

instance F.Storable CS6 where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS6
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS6 cS6_a2 cS6_b3 ->
               F.pokeByteOff ptr0 0 cS6_a2
            >> F.pokeByteOff ptr0 4 cS6_b3
