{-# LANGUAGE NoImplicitPrelude #-}

module VendorCycle.Ctx where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Lib_ctx

instance Eq Lib_ctx

instance BG.Generic Lib_ctx

instance Show Lib_ctx

instance Marshal.StaticSize Lib_ctx

instance Marshal.ReadRaw Lib_ctx

instance Marshal.WriteRaw Lib_ctx

instance BG.Storable Lib_ctx

instance Struct.IsStruct Lib_ctx
