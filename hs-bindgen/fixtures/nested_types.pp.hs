{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @foo@

    __defined at:__ @nested_types.h:1:8@

    __exported by:__ @nested_types.h@
-}
data Foo = Foo
  { foo_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @nested_types.h:2:9@

         __exported by:__ @nested_types.h@
    -}
  , foo_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @nested_types.h:3:10@

         __exported by:__ @nested_types.h@
    -}
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

{-| __C declaration:__ @bar@

    __defined at:__ @nested_types.h:6:8@

    __exported by:__ @nested_types.h@
-}
data Bar = Bar
  { bar_foo1 :: Foo
    {- ^ __C declaration:__ @foo1@

         __defined at:__ @nested_types.h:7:16@

         __exported by:__ @nested_types.h@
    -}
  , bar_foo2 :: Foo
    {- ^ __C declaration:__ @foo2@

         __defined at:__ @nested_types.h:8:16@

         __exported by:__ @nested_types.h@
    -}
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

{-| __defined at:__ @nested_types.h:12:5@

    __exported by:__ @nested_types.h@
-}
data Ex3_ex3_struct = Ex3_ex3_struct
  { ex3_ex3_struct_ex3_a :: FC.CInt
    {- ^ __C declaration:__ @ex3_a@

         __defined at:__ @nested_types.h:13:13@

         __exported by:__ @nested_types.h@
    -}
  , ex3_ex3_struct_ex3_b :: FC.CChar
    {- ^ __C declaration:__ @ex3_b@

         __defined at:__ @nested_types.h:14:14@

         __exported by:__ @nested_types.h@
    -}
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

{-| __C declaration:__ @ex3@

    __defined at:__ @nested_types.h:11:8@

    __exported by:__ @nested_types.h@
-}
data Ex3 = Ex3
  { ex3_ex3_struct :: Ex3_ex3_struct
    {- ^ __C declaration:__ @ex3_struct@

         __defined at:__ @nested_types.h:15:7@

         __exported by:__ @nested_types.h@
    -}
  , ex3_ex3_c :: FC.CFloat
    {- ^ __C declaration:__ @ex3_c@

         __defined at:__ @nested_types.h:16:11@

         __exported by:__ @nested_types.h@
    -}
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

{-| __C declaration:__ @ex4_even@

    __defined at:__ @nested_types.h:24:12@

    __exported by:__ @nested_types.h@
-}
data Ex4_even = Ex4_even
  { ex4_even_value :: FC.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @nested_types.h:25:16@

         __exported by:__ @nested_types.h@
    -}
  , ex4_even_next :: Ptr.Ptr Ex4_odd
    {- ^ __C declaration:__ @next@

         __defined at:__ @nested_types.h:26:25@

         __exported by:__ @nested_types.h@
    -}
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

{-| __C declaration:__ @ex4_odd@

    __defined at:__ @nested_types.h:22:8@

    __exported by:__ @nested_types.h@
-}
data Ex4_odd = Ex4_odd
  { ex4_odd_value :: FC.CInt
    {- ^ __C declaration:__ @value@

         __defined at:__ @nested_types.h:23:9@

         __exported by:__ @nested_types.h@
    -}
  , ex4_odd_next :: Ptr.Ptr Ex4_even
    {- ^ __C declaration:__ @next@

         __defined at:__ @nested_types.h:27:8@

         __exported by:__ @nested_types.h@
    -}
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
