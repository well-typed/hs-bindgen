{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data CS1c = MkCS1c
  { cS1c_a :: FC.CInt
  , cS1c_b :: FC.CInt
  }

instance F.Storable CS1c where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS1c
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS1c cS1c_a2 cS1c_b3 ->
               F.pokeByteOff ptr0 0 cS1c_a2
            >> F.pokeByteOff ptr0 4 cS1c_b3

data CS1 = MkCS1
  { cS1_c :: CS1c
  , cS1_d :: FC.CInt
  }

instance F.Storable CS1 where

  sizeOf = \_ -> 12

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS1
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS1 cS1_c2 cS1_d3 ->
               F.pokeByteOff ptr0 0 cS1_c2
            >> F.pokeByteOff ptr0 8 cS1_d3

data CS2innerdeep = MkCS2innerdeep
  { cS2innerdeep_b :: FC.CInt
  }

instance F.Storable CS2innerdeep where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS2innerdeep
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS2innerdeep cS2innerdeep_b2 -> F.pokeByteOff ptr0 0 cS2innerdeep_b2

data CS2inner = MkCS2inner
  { cS2inner_a :: FC.CInt
  , cS2inner_deep :: CS2innerdeep
  }

instance F.Storable CS2inner where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS2inner
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS2inner cS2inner_a2 cS2inner_deep3 ->
               F.pokeByteOff ptr0 0 cS2inner_a2
            >> F.pokeByteOff ptr0 4 cS2inner_deep3

data CS2 = MkCS2
  { cS2_inner :: CS2inner
  , cS2_d :: FC.CInt
  }

instance F.Storable CS2 where

  sizeOf = \_ -> 12

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS2
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS2 cS2_inner2 cS2_d3 ->
               F.pokeByteOff ptr0 0 cS2_inner2
            >> F.pokeByteOff ptr0 8 cS2_d3
