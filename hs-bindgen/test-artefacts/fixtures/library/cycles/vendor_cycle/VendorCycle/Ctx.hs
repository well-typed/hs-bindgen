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

module VendorCycle.Ctx
    ( VendorCycle.Ctx.Lib_ctx(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified VendorCycle.Vendor
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct lib_ctx@

    __defined at:__ @vendor_cycle\/ctx.h 10:8@

    __exported by:__ @vendor_cycle.h@
-}
data Lib_ctx = Lib_ctx
  { lib_ctx_host :: BG.Ptr VendorCycle.Vendor.Host_api
    {- ^ __C declaration:__ @host@

         __defined at:__ @vendor_cycle\/ctx.h 11:20@

         __exported by:__ @vendor_cycle.h@
    -}
  , lib_ctx_id :: BG.CInt
    {- ^ __C declaration:__ @id@

         __defined at:__ @vendor_cycle\/ctx.h 12:7@

         __exported by:__ @vendor_cycle.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Lib_ctx where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Lib_ctx where

  readRaw =
    \ptr0 ->
          pure Lib_ctx
      <*> HasCField.readRaw (BG.Proxy @"lib_ctx_host") ptr0
      <*> HasCField.readRaw (BG.Proxy @"lib_ctx_id") ptr0

instance Marshal.WriteRaw Lib_ctx where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Lib_ctx lib_ctx_host2 lib_ctx_id3 ->
               HasCField.writeRaw (BG.Proxy @"lib_ctx_host") ptr0 lib_ctx_host2
            >> HasCField.writeRaw (BG.Proxy @"lib_ctx_id") ptr0 lib_ctx_id3

deriving via Marshal.EquivStorable Lib_ctx instance BG.Storable Lib_ctx

deriving via Struct.IsStructViaReadRaw Lib_ctx instance Struct.IsStruct Lib_ctx

{-| __C declaration:__ @host@

    __defined at:__ @vendor_cycle\/ctx.h 11:20@

    __exported by:__ @vendor_cycle.h@
-}
instance ( ty ~ BG.Ptr VendorCycle.Vendor.Host_api
         ) => BG.CompatHasField.HasField "lib_ctx_host" Lib_ctx ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Lib_ctx {lib_ctx_host = y1, lib_ctx_id = BG.getField @"lib_ctx_id" x0}
      , BG.getField @"lib_ctx_host" x0
      )

instance ( ty ~ BG.Ptr VendorCycle.Vendor.Host_api
         ) => BG.HasField "lib_ctx_host" (BG.Ptr Lib_ctx) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"lib_ctx_host")

instance HasCField.HasCField Lib_ctx "lib_ctx_host" where

  type CFieldType Lib_ctx "lib_ctx_host" =
    BG.Ptr VendorCycle.Vendor.Host_api

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @id@

    __defined at:__ @vendor_cycle\/ctx.h 12:7@

    __exported by:__ @vendor_cycle.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "lib_ctx_id" Lib_ctx ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Lib_ctx {lib_ctx_id = y1, lib_ctx_host = BG.getField @"lib_ctx_host" x0}
      , BG.getField @"lib_ctx_id" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "lib_ctx_id" (BG.Ptr Lib_ctx) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"lib_ctx_id")

instance HasCField.HasCField Lib_ctx "lib_ctx_id" where

  type CFieldType Lib_ctx "lib_ctx_id" = BG.CInt

  offset# = \_ -> \_ -> 8
