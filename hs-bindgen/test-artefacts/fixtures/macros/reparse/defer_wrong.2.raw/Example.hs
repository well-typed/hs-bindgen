{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S(..)
    , Example.t
    , Example.Foo(..)
    , Example.Bar(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct S@

    __defined at:__ @macros\/reparse\/defer_wrong.h 11:8@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
data S = S
  { s_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/defer_wrong.h 11:16@

         __exported by:__ @macros\/reparse\/defer_wrong.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 ->
            HasCField.writeRaw (BG.Proxy @"s_x") ptr0 s_x2

deriving via Marshal.EquivStorable S instance BG.Storable S

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/defer_wrong.h 11:16@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_x = y1}, BG.getField @"s_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "s_x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_x")

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/reparse\/defer_wrong.h 13:9@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
t :: [String]
t = ["struct", "S"]

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/defer_wrong.h 15:11@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
newtype Foo = Foo
  { unwrapFoo :: S
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ S) => BG.CompatHasField.HasField "unwrapFoo" Foo ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {unwrapFoo = y1}, BG.getField @"unwrapFoo" x0)

instance (ty ~ S) => BG.HasField "unwrapFoo" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = S

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/defer_wrong.h 16:13@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
newtype Bar = Bar
  { unwrapBar :: BG.Ptr S
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.Ptr S) => BG.CompatHasField.HasField "unwrapBar" Bar ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bar {unwrapBar = y1}, BG.getField @"unwrapBar" x0)

instance ( ty ~ BG.Ptr S
         ) => BG.HasField "unwrapBar" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapBar")

instance HasCField.HasCField Bar "unwrapBar" where

  type CFieldType Bar "unwrapBar" = BG.Ptr S

  offset# = \_ -> \_ -> 0
