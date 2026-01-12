{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified Text.Read
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Enum, Eq, Floating, Fractional, Int, Num, Ord, Read, Real, RealFloat, RealFrac, Show, pure, showsPrec)

{-| __C declaration:__ @enum foo@

    __defined at:__ @types\/typedefs\/typenames.h 14:6@

    __exported by:__ @types\/typedefs\/typenames.h@
-}
newtype Foo = Foo
  { un_Foo :: FC.CUInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

instance F.Storable Foo where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo un_Foo2 -> F.pokeByteOff ptr0 (0 :: Int) un_Foo2

deriving via FC.CUInt instance Data.Primitive.Types.Prim Foo

instance HsBindgen.Runtime.CEnum.CEnum Foo where

  type CEnumZ Foo = FC.CUInt

  toCEnum = Foo

  fromCEnum = un_Foo

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "FOO1")
                                                     , (1, Data.List.NonEmpty.singleton "FOO2")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Foo"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Foo"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Foo where

  minDeclaredValue = FOO1

  maxDeclaredValue = FOO2

instance Show Foo where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Foo where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @FOO1@

    __defined at:__ @types\/typedefs\/typenames.h 15:2@

    __exported by:__ @types\/typedefs\/typenames.h@
-}
pattern FOO1 :: Foo
pattern FOO1 = Foo 0

{-| __C declaration:__ @FOO2@

    __defined at:__ @types\/typedefs\/typenames.h 16:2@

    __exported by:__ @types\/typedefs\/typenames.h@
-}
pattern FOO2 :: Foo
pattern FOO2 = Foo 1

{-| __C declaration:__ @foo@

    __defined at:__ @types\/typedefs\/typenames.h 19:16@

    __exported by:__ @types\/typedefs\/typenames.h@
-}
newtype Foo = Foo
  { un_Foo :: FC.CDouble
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "un_Foo")
         ) => GHC.Records.HasField "un_Foo" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Foo")

instance HsBindgen.Runtime.HasCField.HasCField Foo "un_Foo" where

  type CFieldType Foo "un_Foo" = FC.CDouble

  offset# = \_ -> \_ -> 0
