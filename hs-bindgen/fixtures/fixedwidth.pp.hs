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
    \ptr0 ->
          pure MkCFoo
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCFoo cFoo_sixty_four2 cFoo_thirty_two3 ->
               F.pokeByteOff ptr0 0 cFoo_sixty_four2
            >> F.pokeByteOff ptr0 8 cFoo_thirty_two3
