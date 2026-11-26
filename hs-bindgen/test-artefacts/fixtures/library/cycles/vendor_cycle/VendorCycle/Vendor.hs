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

{-| Copies of declarations from outside the @--library@ directories

    The selection keeps them, usually because program slicing pulls in what the library uses.

    To share them between libraries, generate their bindings once, and pass that binding specification with @--external-binding-spec@ to each library run.
-}
module VendorCycle.Vendor
    ( VendorCycle.Vendor.Host_api(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified VendorCycle.Ctx
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure, type (~))

{-| __C declaration:__ @struct host_api@

    __defined at:__ @vendor\/host.h 9:8@

    __exported by:__ @vendor_cycle.h@
-}
data Host_api = Host_api
  { host_api_ctx :: BG.Ptr VendorCycle.Ctx.Lib_ctx
    {- ^ __C declaration:__ @ctx@

         __defined at:__ @vendor\/host.h 10:19@

         __exported by:__ @vendor_cycle.h@
    -}
  , host_api_log :: BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO BG.CInt)
    {- ^ __C declaration:__ @log@

         __defined at:__ @vendor\/host.h 11:9@

         __exported by:__ @vendor_cycle.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Host_api where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Host_api where

  readRaw =
    \ptr0 ->
          pure Host_api
      <*> HasCField.readRaw (BG.Proxy @"host_api_ctx") ptr0
      <*> HasCField.readRaw (BG.Proxy @"host_api_log") ptr0

instance Marshal.WriteRaw Host_api where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Host_api host_api_ctx2 host_api_log3 ->
               HasCField.writeRaw (BG.Proxy @"host_api_ctx") ptr0 host_api_ctx2
            >> HasCField.writeRaw (BG.Proxy @"host_api_log") ptr0 host_api_log3

deriving via Marshal.EquivStorable Host_api instance BG.Storable Host_api

deriving via Struct.IsStructViaReadRaw Host_api instance Struct.IsStruct Host_api

{-| __C declaration:__ @ctx@

    __defined at:__ @vendor\/host.h 10:19@

    __exported by:__ @vendor_cycle.h@
-}
instance ( ty ~ BG.Ptr VendorCycle.Ctx.Lib_ctx
         ) => BG.CompatHasField.HasField "host_api_ctx" Host_api ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Host_api {host_api_ctx = y1, host_api_log = BG.getField @"host_api_log" x0}
      , BG.getField @"host_api_ctx" x0
      )

instance ( ty ~ BG.Ptr VendorCycle.Ctx.Lib_ctx
         ) => BG.HasField "host_api_ctx" (BG.Ptr Host_api) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"host_api_ctx")

instance HasCField.HasCField Host_api "host_api_ctx" where

  type CFieldType Host_api "host_api_ctx" =
    BG.Ptr VendorCycle.Ctx.Lib_ctx

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @log@

    __defined at:__ @vendor\/host.h 11:9@

    __exported by:__ @vendor_cycle.h@
-}
instance ( ty ~ BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "host_api_log" Host_api ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Host_api {host_api_log = y1, host_api_ctx = BG.getField @"host_api_ctx" x0}
      , BG.getField @"host_api_log" x0
      )

instance ( ty ~ BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO BG.CInt)
         ) => BG.HasField "host_api_log" (BG.Ptr Host_api) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"host_api_log")

instance HasCField.HasCField Host_api "host_api_log" where

  type CFieldType Host_api "host_api_log" =
    BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO BG.CInt)

  offset# = \_ -> \_ -> 8
