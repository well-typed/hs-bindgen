{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S(..)
    , Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct S@

    __defined at:__ @declarations\/field_name_reuse_omit.h 22:8@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
data S = S
  { foo :: BG.CInt
    {- ^ __C declaration:__ @foo@

         __defined at:__ @declarations\/field_name_reuse_omit.h 22:16@

         __exported by:__ @declarations\/field_name_reuse_omit.h@
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
      <*> HasCField.readRaw (BG.Proxy @"foo") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S foo2 ->
            HasCField.writeRaw (BG.Proxy @"foo") ptr0 foo2

deriving via Marshal.EquivStorable S instance BG.Storable S

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/field_name_reuse_omit.h 22:16@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo" S ty where

  hasField =
    \x0 -> (\y1 -> S {foo = y1}, BG.getField @"foo" x0)

instance (ty ~ BG.CInt) => BG.HasField "foo" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo")

instance HasCField.HasCField S "foo" where

  type CFieldType S "foo" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct T@

    __defined at:__ @declarations\/field_name_reuse_omit.h 27:8@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
data T = T
  { bar :: BG.CInt
    {- ^ __C declaration:__ @bar@

         __defined at:__ @declarations\/field_name_reuse_omit.h 27:16@

         __exported by:__ @declarations\/field_name_reuse_omit.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T where

  readRaw =
    \ptr0 ->
          pure T
      <*> HasCField.readRaw (BG.Proxy @"bar") ptr0

instance Marshal.WriteRaw T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T bar2 ->
            HasCField.writeRaw (BG.Proxy @"bar") ptr0 bar2

deriving via Marshal.EquivStorable T instance BG.Storable T

{-| __C declaration:__ @bar@

    __defined at:__ @declarations\/field_name_reuse_omit.h 27:16@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "bar" T ty where

  hasField =
    \x0 -> (\y1 -> T {bar = y1}, BG.getField @"bar" x0)

instance (ty ~ BG.CInt) => BG.HasField "bar" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar")

instance HasCField.HasCField T "bar" where

  type CFieldType T "bar" = BG.CInt

  offset# = \_ -> \_ -> 0
