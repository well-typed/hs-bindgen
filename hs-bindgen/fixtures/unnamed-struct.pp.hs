{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data CX = MkCX
  { cX_foo :: FC.CInt
  , cX_bar :: FC.CInt
  }

instance F.Storable CX where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCX
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 4

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCX cX_foo2 cX_bar3 ->
               F.pokeByteOff x0 0 cX_foo2
            >> F.pokeByteOff x0 4 cX_bar3
