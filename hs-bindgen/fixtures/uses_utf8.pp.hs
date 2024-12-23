{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), pure)

newtype MyEnum = MyEnum
  { unMyEnum :: FC.CUInt
  }

instance F.Storable MyEnum where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MyEnum
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyEnum unMyEnum2 -> F.pokeByteOff ptr0 0 unMyEnum2

pattern Say你好 :: MyEnum
pattern Say你好 = MyEnum 0

pattern Say拜拜 :: MyEnum
pattern Say拜拜 = MyEnum 1
