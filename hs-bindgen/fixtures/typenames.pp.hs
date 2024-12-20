{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), pure)

newtype CFoo = MkCFoo
  { unCFoo :: FC.CUInt
  }

instance F.Storable CFoo where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCFoo
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCFoo unCFoo2 -> F.pokeByteOff ptr0 0 unCFoo2

pattern MkCFOO1 :: CFoo
pattern MkCFOO1 = MkCFoo 0

pattern MkCFOO2 :: CFoo
pattern MkCFOO2 = MkCFoo 1

newtype CFoo = MkCFoo
  { unCFoo :: FC.CDouble
  }

deriving newtype instance F.Storable CFoo
