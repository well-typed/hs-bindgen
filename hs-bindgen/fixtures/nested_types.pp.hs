{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data CFoo = MkCFoo
  { cFoo_i :: FC.CInt
  , cFoo_c :: FC.CChar
  }

instance F.Storable CFoo where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCFoo
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCFoo cFoo_i2 cFoo_c3 ->
               F.pokeByteOff ptr0 0 cFoo_i2
            >> F.pokeByteOff ptr0 4 cFoo_c3

data CBar = MkCBar
  { cBar_foo1 :: CFoo
  , cBar_foo2 :: CFoo
  }

instance F.Storable CBar where

  sizeOf = \_ -> 16

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCBar
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCBar cBar_foo12 cBar_foo23 ->
               F.pokeByteOff ptr0 0 cBar_foo12
            >> F.pokeByteOff ptr0 8 cBar_foo23
