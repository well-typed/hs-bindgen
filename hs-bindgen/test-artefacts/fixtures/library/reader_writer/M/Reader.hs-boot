{-# LANGUAGE NoImplicitPrelude #-}

module M.Reader where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Handle

instance Eq Handle

instance BG.Generic Handle

instance Show Handle

instance Marshal.StaticSize Handle

instance Marshal.ReadRaw Handle

instance Marshal.WriteRaw Handle

instance BG.Storable Handle

instance Struct.IsStruct Handle
