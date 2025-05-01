{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import Prelude ((<*>), (>>), Eq, Int, Show, pure, return)

data Foo

data Bar = Bar
  { bar_ptrA :: F.Ptr Foo
  , bar_ptrB :: F.Ptr Bar
  }

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

deriving stock instance Show Bar

deriving stock instance Eq Bar

data Baz = Baz
  {}

instance F.Storable Baz where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Baz

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz -> return ()

deriving stock instance Show Baz

deriving stock instance Eq Baz

data Quu

data Opaque_union
