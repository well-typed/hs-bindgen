{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

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

  toCEnum = MyEnum

  fromCEnum = un_MyEnum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "Say\20320\22909"), (1, Data.List.NonEmpty.singleton "Say\25308\25308")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "MyEnum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum MyEnum where

  minDeclaredValue = Say你好

  maxDeclaredValue = Say拜拜

instance Show MyEnum where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

pattern Say你好 :: MyEnum
pattern Say你好 = MyEnum 0

pattern Say拜拜 :: MyEnum
pattern Say拜拜 = MyEnum 1
