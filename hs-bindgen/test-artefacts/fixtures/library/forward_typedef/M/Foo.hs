{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module M.Foo
    ( M.Foo.Foo(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.Bar
import {-# SOURCE #-} qualified M.Types
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct foo@

    __defined at:__ @foo.h 5:8@

    __exported by:__ @root.h@
-}
data Foo = Foo
  { foo_owner :: BG.Ptr M.Bar.Bar
    {- ^ __C declaration:__ @owner@

         __defined at:__ @foo.h 5:19@

         __exported by:__ @root.h@
    -}
  , foo_status :: M.Types.Lib_status
    {- ^ __C declaration:__ @status@

         __defined at:__ @foo.h 5:37@

         __exported by:__ @root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (BG.Proxy @"foo_owner") ptr0
      <*> HasCField.readRaw (BG.Proxy @"foo_status") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_owner2 foo_status3 ->
               HasCField.writeRaw (BG.Proxy @"foo_owner") ptr0 foo_owner2
            >> HasCField.writeRaw (BG.Proxy @"foo_status") ptr0 foo_status3

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

deriving via Struct.IsStructViaReadRaw Foo instance Struct.IsStruct Foo

{-| __C declaration:__ @owner@

    __defined at:__ @foo.h 5:19@

    __exported by:__ @root.h@
-}
instance ( ty ~ BG.Ptr M.Bar.Bar
         ) => BG.CompatHasField.HasField "foo_owner" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_owner = y1, foo_status = BG.getField @"foo_status" x0}
      , BG.getField @"foo_owner" x0
      )

instance ( ty ~ BG.Ptr M.Bar.Bar
         ) => BG.HasField "foo_owner" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_owner")

instance HasCField.HasCField Foo "foo_owner" where

  type CFieldType Foo "foo_owner" = BG.Ptr M.Bar.Bar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @status@

    __defined at:__ @foo.h 5:37@

    __exported by:__ @root.h@
-}
instance ( ty ~ M.Types.Lib_status
         ) => BG.CompatHasField.HasField "foo_status" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_status = y1, foo_owner = BG.getField @"foo_owner" x0}
      , BG.getField @"foo_status" x0
      )

instance ( ty ~ M.Types.Lib_status
         ) => BG.HasField "foo_status" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_status")

instance HasCField.HasCField Foo "foo_status" where

  type CFieldType Foo "foo_status" = M.Types.Lib_status

  offset# = \_ -> \_ -> 8
