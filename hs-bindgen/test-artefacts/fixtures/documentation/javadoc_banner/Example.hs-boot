{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Banner_point

instance Eq Banner_point

instance BG.Generic Banner_point

instance Show Banner_point

instance Marshal.StaticSize Banner_point

instance Marshal.ReadRaw Banner_point

instance Marshal.WriteRaw Banner_point

instance BG.Storable Banner_point

instance Struct.IsStruct Banner_point
