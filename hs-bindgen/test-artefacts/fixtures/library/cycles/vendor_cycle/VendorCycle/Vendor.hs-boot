{-# LANGUAGE NoImplicitPrelude #-}

module VendorCycle.Vendor where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Host_api

instance Eq Host_api

instance BG.Generic Host_api

instance Show Host_api

instance Marshal.StaticSize Host_api

instance Marshal.ReadRaw Host_api

instance Marshal.WriteRaw Host_api

instance BG.Storable Host_api

instance Struct.IsStruct Host_api
