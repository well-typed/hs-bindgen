{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data Foo = Foo
  { foo_i :: FC.CInt
  , foo_c :: FC.CChar
  }

instance F.Storable Foo where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_i2 foo_c3 ->
               F.pokeByteOff ptr0 0 foo_i2
            >> F.pokeByteOff ptr0 4 foo_c3

data Bar = Bar
  { bar_foo1 :: Foo
  , bar_foo2 :: Foo
  }

instance F.Storable Bar where

  sizeOf = \_ -> 16

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_foo12 bar_foo23 ->
               F.pokeByteOff ptr0 0 bar_foo12
            >> F.pokeByteOff ptr0 8 bar_foo23

data Ex3 = Ex3
  { ex3_ex3_c :: FC.CFloat
  }

instance F.Storable Ex3 where

  sizeOf = \_ -> 12

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Ex3
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3 ex3_ex3_c2 -> F.pokeByteOff ptr0 8 ex3_ex3_c2

data Ex4_even = Ex4_even
  { ex4_even_ex4_even_value :: FC.CDouble
  , ex4_even_next :: F.Ptr Ex4_odd
  }

instance F.Storable Ex4_even where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure Ex4_even
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_even ex4_even_ex4_even_value2 ex4_even_next3 ->
               F.pokeByteOff ptr0 0 ex4_even_ex4_even_value2
            >> F.pokeByteOff ptr0 8 ex4_even_next3

data Ex4_odd = Ex4_odd
  { ex4_odd_ex4_odd_value :: FC.CInt
  , ex4_odd_next :: F.Ptr Ex4_even
  }

instance F.Storable Ex4_odd where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure Ex4_odd
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_odd ex4_odd_ex4_odd_value2 ex4_odd_next3 ->
               F.pokeByteOff ptr0 0 ex4_odd_ex4_odd_value2
            >> F.pokeByteOff ptr0 8 ex4_odd_next3
