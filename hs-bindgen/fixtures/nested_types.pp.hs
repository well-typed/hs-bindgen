{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as F
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Foo = Foo
  { foo_i :: FC.CInt
  , foo_c :: FC.CChar
  }
  deriving stock (Eq, Show)

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

data Bar = Bar
  { bar_foo1 :: Foo
  , bar_foo2 :: Foo
  }
  deriving stock (Eq, Show)

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

data Ex3_ex3_struct = Ex3_ex3_struct
  { ex3_ex3_struct_ex3_a :: FC.CInt
  , ex3_ex3_struct_ex3_b :: FC.CChar
  }
  deriving stock (Eq, Show)

instance F.Storable Ex3_ex3_struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Ex3_ex3_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3_ex3_struct ex3_ex3_struct_ex3_a2 ex3_ex3_struct_ex3_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) ex3_ex3_struct_ex3_a2
            >> F.pokeByteOff ptr0 (4 :: Int) ex3_ex3_struct_ex3_b3

data Ex3 = Ex3
  { ex3_ex3_struct :: Ex3_ex3_struct
  , ex3_ex3_c :: FC.CFloat
  }
  deriving stock (Eq, Show)

instance F.Storable Ex3 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Ex3
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3 ex3_ex3_struct2 ex3_ex3_c3 ->
               F.pokeByteOff ptr0 (0 :: Int) ex3_ex3_struct2
            >> F.pokeByteOff ptr0 (8 :: Int) ex3_ex3_c3

data Ex4_even = Ex4_even
  { ex4_even_value :: FC.CDouble
  , ex4_even_next :: F.Ptr Ex4_odd
  }
  deriving stock (Eq, Show)

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

data Ex4_odd = Ex4_odd
  { ex4_odd_value :: FC.CInt
  , ex4_odd_next :: F.Ptr Ex4_even
  }
  deriving stock (Eq, Show)

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
