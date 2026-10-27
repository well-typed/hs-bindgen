{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module M.A
    ( M.A.Foo(..)
    , M.A.Bar(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.B
import Prelude ((<*>), Eq, Int, Ord, Show, pure, type (~))

{-| __C declaration:__ @foo@

    __defined at:__ @a.h 6:21@

    __exported by:__ @b.h@
-}
newtype Foo = Foo
  { unwrapFoo :: BG.Ptr M.B.Foo
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr M.B.Foo
         ) => BG.CompatHasField.HasField "unwrapFoo" Foo ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {unwrapFoo = y1}, BG.getField @"unwrapFoo" x0)

instance ( ty ~ BG.Ptr M.B.Foo
         ) => BG.HasField "unwrapFoo" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = BG.Ptr M.B.Foo

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct bar@

    __defined at:__ @a.h 9:8@

    __exported by:__ @b.h@
-}
data Bar = Bar
  { bar_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @a.h 9:18@

         __exported by:__ @b.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (BG.Proxy @"bar_x") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 ->
            HasCField.writeRaw (BG.Proxy @"bar_x") ptr0 bar_x2

deriving via Marshal.EquivStorable Bar instance BG.Storable Bar

deriving via Struct.IsStructViaReadRaw Bar instance Struct.IsStruct Bar

{-| __C declaration:__ @x@

    __defined at:__ @a.h 9:18@

    __exported by:__ @b.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "bar_x" Bar ty where

  hasField =
    \x0 ->
      (\y1 -> Bar {bar_x = y1}, BG.getField @"bar_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "bar_x" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_x")

instance HasCField.HasCField Bar "bar_x" where

  type CFieldType Bar "bar_x" = BG.CInt

  offset# = \_ -> \_ -> 0
