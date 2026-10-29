{-# LANGUAGE NoImplicitPrelude #-}

module M.A where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data A

instance Eq A

instance BG.Generic A

instance Show A

instance Marshal.StaticSize A

instance Marshal.ReadRaw A

instance Marshal.WriteRaw A

instance BG.Storable A

instance Struct.IsStruct A
