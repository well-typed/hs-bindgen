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
    \x0 ->
          pure MkCMyEnum
      <*> F.peekByteOff x0 0

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCMyEnum unCMyEnum2 -> F.pokeByteOff x0 0 unCMyEnum2
