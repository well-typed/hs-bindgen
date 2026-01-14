{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified Text.Read
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum MyEnum@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
newtype MyEnum = MyEnum
  { un_MyEnum :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

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

deriving via FC.CUInt instance Data.Primitive.Types.Prim MyEnum

instance HsBindgen.Runtime.CEnum.CEnum MyEnum where

  type CEnumZ MyEnum = FC.CUInt

  toCEnum = MyEnum

  fromCEnum = un_MyEnum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "X")]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "MyEnum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "MyEnum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum MyEnum where

  minDeclaredValue = X

  maxDeclaredValue = X

instance Show MyEnum where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read MyEnum where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 4:14@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
pattern X :: MyEnum
pattern X = MyEnum 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
newtype A = A
  { un_A :: MyEnum
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
newtype B = B
  { un_B :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)
