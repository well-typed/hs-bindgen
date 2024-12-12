{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

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

pattern MkCSay你好 :: CMyEnum
pattern MkCSay你好 = MkCMyEnum 0

pattern MkCSay拜拜 :: CMyEnum
pattern MkCSay拜拜 = MkCMyEnum 1
