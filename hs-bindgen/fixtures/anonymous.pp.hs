{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data S1_c = S1_c
  { s1_c_a :: FC.CInt
  , s1_c_b :: FC.CInt
  }

instance F.Storable S1_c where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1_c
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_c s1_c_a2 s1_c_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s1_c_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s1_c_b3

deriving stock instance Show S1_c

deriving stock instance Eq S1_c

data S1 = S1
  { s1_c :: S1_c
  , s1_d :: FC.CInt
  }

instance F.Storable S1 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_c2 s1_d3 ->
               F.pokeByteOff ptr0 (0 :: Int) s1_c2
            >> F.pokeByteOff ptr0 (8 :: Int) s1_d3

deriving stock instance Show S1

deriving stock instance Eq S1

data S2_inner_deep = S2_inner_deep
  { s2_inner_deep_b :: FC.CInt
  }

instance F.Storable S2_inner_deep where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_inner_deep
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner_deep s2_inner_deep_b2 -> F.pokeByteOff ptr0 (0 :: Int) s2_inner_deep_b2

deriving stock instance Show S2_inner_deep

deriving stock instance Eq S2_inner_deep

data S2_inner = S2_inner
  { s2_inner_a :: FC.CInt
  , s2_inner_deep :: S2_inner_deep
  }

instance F.Storable S2_inner where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_inner
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner s2_inner_a2 s2_inner_deep3 ->
               F.pokeByteOff ptr0 (0 :: Int) s2_inner_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s2_inner_deep3

deriving stock instance Show S2_inner

deriving stock instance Eq S2_inner

data S2 = S2
  { s2_inner :: S2_inner
  , s2_d :: FC.CInt
  }

instance F.Storable S2 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_inner2 s2_d3 ->
               F.pokeByteOff ptr0 (0 :: Int) s2_inner2
            >> F.pokeByteOff ptr0 (8 :: Int) s2_d3

deriving stock instance Show S2

deriving stock instance Eq S2
