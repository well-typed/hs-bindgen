{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), pure)

newtype CMyEnum = MkCMyEnum
  { unCMyEnum :: FC.CUInt
  }

instance F.Storable CMyEnum where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCMyEnum
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCMyEnum unCMyEnum2 -> F.pokeByteOff ptr0 0 unCMyEnum2
