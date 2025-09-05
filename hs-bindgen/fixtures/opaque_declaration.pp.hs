{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified GHC.Ptr as F
import Prelude ((<*>), (>>), Eq, Int, Show, pure, return)

data Foo

data Bar = Bar
  { bar_ptrA :: F.Ptr Foo
  , bar_ptrB :: F.Ptr Bar
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_ptrA2 bar_ptrB3 ->
               F.pokeByteOff ptr0 (0 :: Int) bar_ptrA2
            >> F.pokeByteOff ptr0 (8 :: Int) bar_ptrB3

data Baz = Baz
  {}
  deriving stock (Eq, Show)

instance F.Storable Baz where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Baz

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz -> return ()

data Quu

data Opaque_union
