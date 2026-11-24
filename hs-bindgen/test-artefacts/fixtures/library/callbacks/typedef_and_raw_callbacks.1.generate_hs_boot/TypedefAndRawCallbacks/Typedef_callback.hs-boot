{-# LANGUAGE NoImplicitPrelude #-}

module TypedefAndRawCallbacks.Typedef_callback where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data Event

instance Eq Event

instance BG.Generic Event

instance Show Event

instance Marshal.StaticSize Event

instance Marshal.ReadRaw Event

instance Marshal.WriteRaw Event

instance BG.Storable Event

instance Struct.IsStruct Event

data Typedef_callback_Aux

instance BG.Generic Typedef_callback_Aux

data Typedef_callback

instance Eq Typedef_callback

instance BG.Generic Typedef_callback

instance Ord Typedef_callback

instance Show Typedef_callback

instance BG.HasFFIType Typedef_callback

instance Marshal.ReadRaw Typedef_callback

instance Marshal.StaticSize Typedef_callback

instance BG.Storable Typedef_callback

instance Marshal.WriteRaw Typedef_callback
