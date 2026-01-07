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

{-| __C declaration:__ @enum E@

    __defined at:__ @binding-specs\/fun_arg\/enum.h:1:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
newtype E = E
  { un_E :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance F.Storable E where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure E
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          E un_E2 -> F.pokeByteOff ptr0 (0 :: Int) un_E2

deriving via FC.CUInt instance Data.Primitive.Types.Prim E

instance HsBindgen.Runtime.CEnum.CEnum E where

  type CEnumZ E = FC.CUInt

  toCEnum = E

  fromCEnum = un_E

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "X")]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "E"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "E"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum E where

  minDeclaredValue = X

  maxDeclaredValue = X

instance Show E where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read E where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/fun_arg\/enum.h:1:10@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
pattern X :: E
pattern X = E 0
