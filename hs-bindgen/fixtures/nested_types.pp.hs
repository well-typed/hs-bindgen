{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Foo = Foo
  { foo_i :: FC.CInt
  , foo_c :: FC.CChar
  }

instance F.Storable Foo where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_i2 foo_c3 ->
               F.pokeByteOff ptr0 (0 :: Int) foo_i2
            >> F.pokeByteOff ptr0 (4 :: Int) foo_c3

deriving stock instance Show Foo

deriving stock instance Eq Foo

data Bar = Bar
  { bar_foo1 :: Foo
  , bar_foo2 :: Foo
  }

instance F.Storable Bar where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_foo12 bar_foo23 ->
               F.pokeByteOff ptr0 (0 :: Int) bar_foo12
            >> F.pokeByteOff ptr0 (8 :: Int) bar_foo23

deriving stock instance Show Bar

deriving stock instance Eq Bar

data Ex3 = Ex3
  { ex3_ex3_c :: FC.CFloat
  }

instance F.Storable Ex3 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Ex3
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3 ex3_ex3_c2 -> F.pokeByteOff ptr0 (8 :: Int) ex3_ex3_c2

deriving stock instance Show Ex3

deriving stock instance Eq Ex3

data Ex4_even = Ex4_even
  { ex4_even_value :: FC.CDouble
  , ex4_even_next :: F.Ptr Ex4_odd
  }

instance F.Storable Ex4_even where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Ex4_even
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_even ex4_even_value2 ex4_even_next3 ->
               F.pokeByteOff ptr0 (0 :: Int) ex4_even_value2
            >> F.pokeByteOff ptr0 (8 :: Int) ex4_even_next3

deriving stock instance Show Ex4_even

deriving stock instance Eq Ex4_even

data Ex4_odd = Ex4_odd
  { ex4_odd_value :: FC.CInt
  , ex4_odd_next :: F.Ptr Ex4_even
  }

instance F.Storable Ex4_odd where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Ex4_odd
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_odd ex4_odd_value2 ex4_odd_next3 ->
               F.pokeByteOff ptr0 (0 :: Int) ex4_odd_value2
            >> F.pokeByteOff ptr0 (8 :: Int) ex4_odd_next3

deriving stock instance Show Ex4_odd

deriving stock instance Eq Ex4_odd
