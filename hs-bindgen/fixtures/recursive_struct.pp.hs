{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data CLinkedListAS = MkCLinkedListAS
  { cLinkedListAS_x :: FC.CInt
  , cLinkedListAS_next :: F.Ptr CLinkedListAS
  }

instance F.Storable CLinkedListAS where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure MkCLinkedListAS
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCLinkedListAS cLinkedListAS_x2 cLinkedListAS_next3 ->
               F.pokeByteOff ptr0 0 cLinkedListAS_x2
            >> F.pokeByteOff ptr0 8 cLinkedListAS_next3

newtype CLinkedListAT = MkCLinkedListAT
  { unCLinkedListAT :: CLinkedListAS
  }

deriving newtype instance F.Storable CLinkedListAT

data CLinkedListBT = MkCLinkedListBT
  { cLinkedListBT_x :: FC.CInt
  , cLinkedListBT_next :: F.Ptr CLinkedListBT
  }

instance F.Storable CLinkedListBT where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure MkCLinkedListBT
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCLinkedListBT cLinkedListBT_x2 cLinkedListBT_next3 ->
               F.pokeByteOff ptr0 0 cLinkedListBT_x2
            >> F.pokeByteOff ptr0 8 cLinkedListBT_next3
