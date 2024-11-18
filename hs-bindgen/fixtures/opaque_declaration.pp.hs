{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import Prelude ((()), (<*>), (>>), pure, return)

data CFoo

data CFoo

data CBar = MkCBar
  { cBar_ptrA :: F.Ptr CStruct'0020foo
  , cBar_ptrB :: F.Ptr CStruct'0020bar
  }

instance F.Storable CBar where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure MkCBar
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCBar cBar_ptrA2 cBar_ptrB3 ->
               F.pokeByteOff ptr0 0 cBar_ptrA2
            >> F.pokeByteOff ptr0 8 cBar_ptrB3

data CBaz = MkCBaz
  {}

instance F.Storable CBaz where

  sizeOf = \_ -> 0

  alignment = \_ -> 1

  peek = \ptr0 -> pure MkCBaz

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCBaz -> return (())
