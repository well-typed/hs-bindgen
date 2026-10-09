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
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct foo@

    __defined at:__ @foo.h 2:8@

    __exported by:__ @callback\/register.h@
-}
data Foo = Foo
  { foo_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @foo.h 3:7@

         __exported by:__ @callback\/register.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (BG.Proxy @"foo_x") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_x2 ->
            HasCField.writeRaw (BG.Proxy @"foo_x") ptr0 foo_x2

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

deriving via Struct.IsStructViaReadRaw Foo instance Struct.IsStruct Foo

{-| __C declaration:__ @x@

    __defined at:__ @foo.h 3:7@

    __exported by:__ @callback\/register.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_x" Foo ty where

  hasField =
    \x0 ->
      (\y1 -> Foo {foo_x = y1}, BG.getField @"foo_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "foo_x" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_x")

instance HasCField.HasCField Foo "foo_x" where

  type CFieldType Foo "foo_x" = BG.CInt

  offset# = \_ -> \_ -> 0
