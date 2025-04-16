{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.Map.Strict
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, show)
import qualified Prelude as P

newtype MyEnum = MyEnum
  { un_MyEnum :: FC.CUInt
  }

instance F.Storable MyEnum where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure MyEnum
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyEnum un_MyEnum2 -> F.pokeByteOff ptr0 (0 :: Int) un_MyEnum2

deriving stock instance Eq MyEnum

deriving stock instance Ord MyEnum

deriving stock instance Read MyEnum

instance HsBindgen.Runtime.CEnum.CEnum MyEnum where

  type CEnumZ MyEnum = FC.CUInt

  fromCEnumZ = MyEnum

  toCEnumZ = un_MyEnum

  declaredValues =
    \_ ->
      Data.Map.Strict.fromList [(0, pure "Say\20320\22909"), (1, pure "Say\25308\25308")]

  rangeIsSequential = \_ -> P.Just (0, 1)

instance Show MyEnum where

  show = HsBindgen.Runtime.CEnum.showCEnum "MyEnum"

pattern Say你好 :: MyEnum
pattern Say你好 = MyEnum 0

pattern Say拜拜 :: MyEnum
pattern Say拜拜 = MyEnum 1
