{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Foo(..)
    , pattern Example.Bar
    , pattern Example.Baz
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @enum foo@

    __defined at:__ @binding-specs\/enum\/open.h 1:6@

    __exported by:__ @binding-specs\/enum\/open.h@
-}
newtype Foo = Foo
  { unwrapFoo :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo unwrapFoo2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapFoo2

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

deriving via BG.CUInt instance BG.Prim Foo

instance CEnum.CEnum Foo where

  type CEnumZ Foo = BG.CUInt

  toCEnum = Foo

  fromCEnum = BG.getField @"unwrapFoo"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "Bar"), (1, BG.singleton "Baz")]

  showsUndeclared = CEnum.showsWrappedUndeclared "Foo"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Foo"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Foo where

  minDeclaredValue = Bar

  maxDeclaredValue = Baz

instance Show Foo where

  showsPrec = CEnum.shows

instance Read Foo where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapFoo" Foo ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {unwrapFoo = y1}, BG.getField @"unwrapFoo" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapFoo" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/enum\/open.h 2:3@

    __exported by:__ @binding-specs\/enum\/open.h@
-}
pattern Bar :: Foo
pattern Bar = Foo 0

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/enum\/open.h 3:3@

    __exported by:__ @binding-specs\/enum\/open.h@
-}
pattern Baz :: Foo
pattern Baz = Foo 1
