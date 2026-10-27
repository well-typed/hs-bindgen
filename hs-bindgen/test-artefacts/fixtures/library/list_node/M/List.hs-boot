{-# LANGUAGE NoImplicitPrelude #-}

module M.List where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data List

instance Eq List

instance BG.Generic List

instance Show List

instance Marshal.StaticSize List

instance Marshal.ReadRaw List

instance Marshal.WriteRaw List

instance BG.Storable List

instance Struct.IsStruct List
