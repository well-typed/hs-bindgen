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

data Ex4_b = Ex4_b
  { ex4_b_ex3_a :: FC.CInt
  , ex4_b_ex3_b :: FC.CChar
  , ex4_b_recur :: F.Ptr Ex4_b
  }

instance F.Storable Ex4_b where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure Ex4_b
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_b ex4_b_ex3_a2 ex4_b_ex3_b3 ex4_b_recur4 ->
               F.pokeByteOff ptr0 0 ex4_b_ex3_a2
            >> F.pokeByteOff ptr0 4 ex4_b_ex3_b3
            >> F.pokeByteOff ptr0 8 ex4_b_recur4

data Ex4 = Ex4
  { ex4_linkedlist :: Ex4_b
  , ex4_ex3_c :: FC.CFloat
  }

instance F.Storable Ex4 where

  sizeOf = \_ -> 24

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure Ex4
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 16

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4 ex4_linkedlist2 ex4_ex3_c3 ->
               F.pokeByteOff ptr0 0 ex4_linkedlist2
            >> F.pokeByteOff ptr0 16 ex4_ex3_c3
