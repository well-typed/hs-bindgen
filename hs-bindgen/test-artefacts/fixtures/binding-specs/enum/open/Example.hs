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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @enum foo@

    __defined at:__ @binding-specs\/enum\/open.h 1:6@

    __exported by:__ @binding-specs\/enum\/open.h@
-}
newtype Foo = Foo
  { unwrapFoo :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

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

deriving via Marshal.EquivStorable Foo instance RIP.Storable Foo

deriving via RIP.CUInt instance RIP.Prim Foo

instance CEnum.CEnum Foo where

  type CEnumZ Foo = RIP.CUInt

  toCEnum = Foo

  fromCEnum = RIP.getField @"unwrapFoo"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "Bar"), (1, RIP.singleton "Baz")]

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

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.CompatHasField.HasField "unwrapFoo" Foo ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {unwrapFoo = y1}, RIP.getField @"unwrapFoo" x0)

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapFoo" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = RIP.CUInt

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
