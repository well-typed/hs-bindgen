{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import qualified Text.Read
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @MyEnum@

    __defined at:__ @uses_utf8.h:4:6@

    __exported by:__ @uses_utf8.h@
-}
newtype MyEnum = MyEnum
  { un_MyEnum :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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
          MyEnum un_MyEnum2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_MyEnum2

instance HsBindgen.Runtime.CEnum.CEnum MyEnum where

  type CEnumZ MyEnum = FC.CUInt

  toCEnum = MyEnum

  fromCEnum = un_MyEnum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "Say\20320\22909")
                                                     , (1, Data.List.NonEmpty.singleton "Say\25308\25308")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "MyEnum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "MyEnum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum MyEnum where

  minDeclaredValue = Say你好

  maxDeclaredValue = Say拜拜

instance Show MyEnum where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read MyEnum where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @Say你好@

    __defined at:__ @uses_utf8.h:5:9@

    __exported by:__ @uses_utf8.h@
-}
pattern Say你好 :: MyEnum
pattern Say你好 = MyEnum 0

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @uses_utf8.h:6:9@

    __exported by:__ @uses_utf8.h@
-}
pattern Say拜拜 :: MyEnum
pattern Say拜拜 = MyEnum 1
