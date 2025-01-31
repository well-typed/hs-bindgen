{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Enum, Eq, Ord, Read, Show, pure)

newtype Foo = Foo
  { unFoo :: FC.CUInt
  }

instance F.Storable Foo where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo unFoo2 -> F.pokeByteOff ptr0 0 unFoo2

deriving stock instance Show Foo

deriving stock instance Read Foo

deriving stock instance Eq Foo

deriving stock instance Ord Foo

deriving newtype instance Enum Foo

pattern FOO1 :: Foo
pattern FOO1 = Foo 0

pattern FOO2 :: Foo
pattern FOO2 = Foo 1

newtype Foo = Foo
  { unFoo :: FC.CDouble
  }

deriving newtype instance F.Storable Foo
