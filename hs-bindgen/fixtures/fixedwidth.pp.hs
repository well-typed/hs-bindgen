{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import Prelude ((<*>), (>>), pure)

data CFoo = MkCFoo
  { cFoo_sixty_four :: CUint64T
  , cFoo_thirty_two :: CUint32T
  }

instance F.Storable CFoo where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \x0 ->
          pure MkCFoo
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 64

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCFoo cFoo_sixty_four2 cFoo_thirty_two3 ->
               F.pokeByteOff x0 0 cFoo_sixty_four2
            >> F.pokeByteOff x0 64 cFoo_thirty_two3
